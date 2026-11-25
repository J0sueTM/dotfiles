import type { Register } from 'claude-code'

// i3 / alacritty palette
const C = {
  surface: '#141918',
  raised: '#1f2726',
  bubble: '#163029', // your own messages: a dark teal tint
  border: '#2a3331',
  muted: '#6f7d78',
  fg: '#dfe3dc',
  gold: '#e6b450',
  teal: '#5fb3a1',
  green: '#9ccf7f',
  red: '#e5675f',
  // diff rows: soft tints of green and red over the surface
  addBg: '#15241b',
  addGutter: '#5c8f58',
  delBg: '#2a1a19',
  delGutter: '#a0554f',
  delText: '#b39c98',
  context: '#a3ada7',
  gutter: '#4a5653',
}

// Nerd Font icons (the font has no ✎ ✓ and friends, so those would come from a fallback font)
const ICON = { pencil: '\uf040', plus: '\uf067', check: '\uf00c' }

// Long enough for any terminal; drawn with wrap="clip" so it ends at the box's edge.
const EDGE = 400

const formatDuration = (ms: number): string => {
  const total = Math.round(ms / 1000)
  const h = Math.floor(total / 3600)
  const m = Math.floor((total % 3600) / 60)
  const s = total % 60
  if (h > 0) return `${h}h ${m}m`
  if (m > 0) return `${m}m ${s}s`
  return `${s}s`
}

type Hunk = { oldStart: number; oldLines: number; newStart: number; newLines: number; lines: string[] }
type FileChange = { filePath: string; structuredPatch: Hunk[]; type?: 'create' | 'update'; content?: string }

const DIFF_LINE_BUDGET = 40
const FILE_TOOLS = new Set(['Edit', 'Write', 'MultiEdit'])

const isFileChange = (output: unknown): output is FileChange =>
  typeof output === 'object' && output !== null &&
  typeof (output as FileChange).filePath === 'string' && Array.isArray((output as FileChange).structuredPatch)

type DiffRow = { kind: 'add' | 'del' | 'ctx' | 'gap'; no: string; text: string }

// Hunks as rows with their line numbers, cut to a budget; `hidden` counts what was left out.
const diffRows = (hunks: Hunk[]): { rows: DiffRow[]; hidden: number } => {
  const rows: DiffRow[] = []
  let hidden = 0
  hunks.forEach((hunk, index) => {
    let oldNo = hunk.oldStart
    let newNo = hunk.newStart
    if (index > 0 && rows.length < DIFF_LINE_BUDGET) rows.push({ kind: 'gap', no: '', text: '' })
    for (const line of hunk.lines) {
      const sign = line[0]
      const kind = sign === '+' ? 'add' : sign === '-' ? 'del' : 'ctx'
      const no = kind === 'del' ? oldNo : newNo
      if (kind !== 'add') oldNo += 1
      if (kind !== 'del') newNo += 1
      if (line.startsWith('\\')) continue
      if (rows.length >= DIFF_LINE_BUDGET) {
        hidden += 1
        continue
      }
      rows.push({ kind, no: String(no), text: line.slice(1).replace(/\t/g, '  ') })
    }
  })
  return { rows, hidden }
}

// File name apart from its folder; the folder shortened to ~ or its last two parts.
const prettyPath = (path: string): { dir: string; name: string } => {
  const cut = path.lastIndexOf('/')
  if (cut < 0) return { dir: '', name: path }
  const name = path.slice(cut + 1)
  let dir = path.slice(0, cut).replace(/^\/home\/[^/]+/, '~')
  const parts = dir.split('/')
  if (parts.length > 4) dir = `…/${parts.slice(-2).join('/')}`
  return { dir, name }
}

// What the spinner says, by what the turn is doing: calm words instead of whimsy.
const SPINNER_WORDS: Record<string, string> = {
  requesting: 'Waiting',
  thinking: 'Thinking',
  responding: 'Writing',
  'tool-input': 'Preparing',
  'tool-use': 'Working',
}

export const register: Register = on => {
  // File edits: a filled card with the file, +/- counts and the diff.
  on('ui.render', { component: 'ToolUse' }, ($, e, next) => {
    const p = e.props
    if (e.surface !== 'terminal' || !FILE_TOOLS.has(p.tool) || p.isRunning || p.isErrored ||
        p.isInterrupted || p.isExpanded || !isFileChange(p.output)) {
      return next(e)
    }
    const { Box, Text, Code } = $.ui.resolve(e)
    const change = p.output
    const created = change.type === 'create' || change.structuredPatch.length === 0
    const lines = change.structuredPatch.flatMap(h => h.lines)
    const added = created && change.content !== undefined
      ? change.content.split('\n').length
      : lines.filter(l => l.startsWith('+')).length
    const removed = created ? 0 : lines.filter(l => l.startsWith('-')).length
    const { dir, name } = prettyPath(change.filePath)

    let body
    let hidden = 0
    if (created && change.content !== undefined) {
      const all = change.content.replace(/\n$/, '').split('\n')
      hidden = Math.max(0, all.length - DIFF_LINE_BUDGET)
      body = (
        <Box backgroundColor={C.surface} paddingX={1}>
          <Code source={all.slice(0, DIFF_LINE_BUDGET).join('\n')} path={change.filePath} startLine={1} wrap="truncate-end" />
        </Box>
      )
    } else {
      const diff = diffRows(change.structuredPatch)
      hidden = diff.hidden
      const pad = Math.max(3, ...diff.rows.map(row => row.no.length))
      body = diff.rows.map((row, i) => {
        if (row.kind === 'gap') {
          return (
            <Box key={`r${i}`} backgroundColor={C.surface} paddingX={1}>
              <Text color={C.gutter} backgroundColor={C.surface}>{`${' '.repeat(pad - 1)}···`}</Text>
            </Box>
          )
        }
        const bg = row.kind === 'add' ? C.addBg : row.kind === 'del' ? C.delBg : C.surface
        const gutter = row.kind === 'add' ? C.addGutter : row.kind === 'del' ? C.delGutter : C.gutter
        const sign = row.kind === 'add' ? '+' : row.kind === 'del' ? '−' : ' '
        const color = row.kind === 'add' ? C.fg : row.kind === 'del' ? C.delText : C.context
        return (
          <Box key={`r${i}`} flexDirection="row" backgroundColor={bg} paddingX={1}>
            <Box flexShrink={0}>
              <Text color={gutter} backgroundColor={bg}>{`${row.no.padStart(pad)} ${sign} `}</Text>
            </Box>
            <Text color={color} backgroundColor={bg} wrap="truncate-end">{row.text === '' ? ' ' : row.text}</Text>
          </Box>
        )
      })
    }

    return (
      <Box flexDirection="column">
        <Text color={C.raised} wrap="clip">{'▄'.repeat(EDGE)}</Text>
        <Box flexDirection="row" justifyContent="space-between" backgroundColor={C.raised} paddingX={2}>
          <Box flexDirection="row" flexShrink={1}>
            <Text color={C.gold} backgroundColor={C.raised}>{`${created ? ICON.plus : ICON.pencil}  `}</Text>
            <Text color={C.fg} backgroundColor={C.raised} bold>{name}</Text>
            <Text color={C.muted} backgroundColor={C.raised} wrap="truncate-start">{dir === '' ? '' : `  ${dir}`}</Text>
          </Box>
          <Box flexDirection="row" flexShrink={0}>
            <Text color={C.green} backgroundColor={C.raised}>{`+${added}`}</Text>
            {removed > 0 && <Text color={C.red} backgroundColor={C.raised}>{` −${removed}`}</Text>}
          </Box>
        </Box>
        {body}
        {hidden > 0 && (
          <Box backgroundColor={C.surface} paddingX={2}>
            <Text color={C.muted} backgroundColor={C.surface}>{`… ${hidden} more lines · ctrl+o to expand`}</Text>
          </Box>
        )}
        <Text color={C.surface} wrap="clip">{'▀'.repeat(EDGE)}</Text>
      </Box>
    )
  })

  // The card above already shows the change; drop the engine's own diff under the row.
  on('ui.render', { component: 'ToolResult' }, ($, e, next) => {
    if (e.surface !== 'terminal' || !FILE_TOOLS.has(e.props.tool) || e.props.isErrored ||
        e.props.isExpanded || !isFileChange(e.props.output)) {
      return next(e)
    }
    const { Box } = $.ui.resolve(e)
    return <Box />
  })

  // Your prompts: a filled bubble on the right, like a chat app.
  on('ui.render', { component: 'UserMessage' }, ($, e, next) => {
    if (e.surface !== 'terminal' || e.props.origin.kind !== 'composer') {
      return next(e)
    }
    const { Box, Text } = $.ui.resolve(e)
    const columns = e.viewport?.columns ?? 100
    const longest = Math.max(...e.props.text.split('\n').map(line => line.length))
    const width = Math.max(8, Math.min(longest, Math.floor(columns * 0.72) - 4)) + 4

    return (
      <Box flexDirection="row" justifyContent="flex-end" marginTop={1}>
        <Box flexDirection="column" width={width}>
          <Text color={C.bubble}>{'▄'.repeat(width)}</Text>
          <Box backgroundColor={C.bubble} paddingX={2}>
            <Text color={C.fg} backgroundColor={C.bubble}>{e.props.text}</Text>
          </Box>
          <Text color={C.bubble}>{'▀'.repeat(width)}</Text>
        </Box>
      </Box>
    )
  })

  // End of a turn: a quiet "Worked for 2m 10s" divider.
  on('ui.render', { component: 'TurnDuration' }, ($, e, next) => {
    if (e.surface !== 'terminal' || e.props.isExpanded || e.props.waitingFor !== undefined) {
      return next(e)
    }
    const { Box, Text } = $.ui.resolve(e)

    return (
      <Box marginTop={1} flexDirection="row">
        <Box flexShrink={0}>
          <Text color={C.teal}>{`${ICON.check}  `}</Text>
          <Text color={C.muted}>{`Worked for ${formatDuration(e.props.durationMs)} `}</Text>
        </Box>
        <Box flexShrink={1} flexGrow={1}><Text color={C.border} wrap="clip">{'─'.repeat(EDGE)}</Text></Box>
      </Box>
    )
  })

  // Code blocks: a filled card with the language in its header.
  on('ui.render', { component: 'CodeBlock' }, ($, e, next) => {
    if (e.surface !== 'terminal' || !e.props.isClosed) {
      return next(e)
    }
    const { Box, Text, Code } = $.ui.resolve(e)
    const source = e.props.source.replace(/\n$/, '')
    const count = source.split('\n').length

    return (
      <Box flexDirection="column">
        <Text color={C.raised} wrap="clip">{'▄'.repeat(EDGE)}</Text>
        <Box flexDirection="row" justifyContent="space-between" backgroundColor={C.raised} paddingX={2}>
          <Text color={C.teal} backgroundColor={C.raised}>{e.props.language === '' ? 'text' : e.props.language}</Text>
          <Text color={C.muted} backgroundColor={C.raised}>{`${count} ${count === 1 ? 'line' : 'lines'}`}</Text>
        </Box>
        <Box backgroundColor={C.surface} paddingX={2}>
          <Code source={source} language={e.props.language || undefined} />
        </Box>
        <Text color={C.surface} wrap="clip">{'▀'.repeat(EDGE)}</Text>
      </Box>
    )
  })

  // The spinner says what the turn is doing.
  on('ui.render', { component: 'Spinner' }, ($, e, next) => {
    if (e.surface !== 'terminal') {
      return next(e)
    }
    const word = SPINNER_WORDS[e.props.mode]
    return word === undefined ? next(e) : next({ ...e, props: { ...e.props, word } })
  })
}
