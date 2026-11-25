import type { Register } from 'claude-code'

// i3 / alacritty palette
const C = {
  surface: '#141918',
  raised: '#1f2726',
  border: '#2a3331',
  muted: '#6f7d78',
  fg: '#dfe3dc',
  gold: '#e6b450',
  teal: '#5fb3a1',
  green: '#9ccf7f',
  red: '#e5675f',
}

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

// Hunks as unified-diff text, cut to a line budget; a cut hunk gets its counts recomputed so it still parses.
const diffText = (hunks: Hunk[]): { text: string; hidden: number } => {
  const out: string[] = []
  let budget = DIFF_LINE_BUDGET
  let hidden = 0
  for (const hunk of hunks) {
    if (budget <= 0) {
      hidden += hunk.lines.length
      continue
    }
    const lines = hunk.lines.slice(0, budget)
    hidden += hunk.lines.length - lines.length
    budget -= lines.length
    const oldLines = lines.filter(l => !l.startsWith('+')).length
    const newLines = lines.filter(l => !l.startsWith('-')).length
    out.push(`@@ -${hunk.oldStart},${oldLines} +${hunk.newStart},${newLines} @@`, ...lines)
  }
  return { text: out.join('\n'), hidden }
}

const prettyPath = (path: string): { dir: string; name: string } => {
  const short = path.replace(/^\/home\/[^/]+\//, '~/')
  const cut = short.lastIndexOf('/')
  return cut < 0 ? { dir: '', name: short } : { dir: short.slice(0, cut + 1), name: short.slice(cut + 1) }
}

export const register: Register = on => {
  // File edits: a card with the file, +/- counts and the diff, like Orca's changed-files card.
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
      body = <Code source={all.slice(0, DIFF_LINE_BUDGET).join('\n')} path={change.filePath} startLine={1} wrap="truncate-end" />
    } else {
      const diff = diffText(change.structuredPatch)
      hidden = diff.hidden
      body = <Code source={diff.text} path={change.filePath} format="diff" wrap="truncate-end" />
    }

    return (
      <Box flexDirection="column" borderStyle="round" borderColor={C.border} paddingX={1} marginY={1}>
        <Box flexDirection="row" justifyContent="space-between">
          <Box flexDirection="row" flexShrink={1}>
            <Text color={C.gold}>{created ? '+ ' : '✎ '}</Text>
            <Text color={C.fg} bold>{name}</Text>
            <Text color={C.muted} wrap="truncate-start">{dir === '' ? '' : `  ${dir}`}</Text>
          </Box>
          <Box flexDirection="row" flexShrink={0}>
            <Text color={C.green}>{`+${added}`}</Text>
            {removed > 0 && <Text color={C.red}>{` -${removed}`}</Text>}
          </Box>
        </Box>
        <Text color={C.border}>{'─'.repeat(Math.max(4, (e.viewport?.columns ?? 100) - 8))}</Text>
        {body}
        {hidden > 0 && <Text color={C.muted}>{`… ${hidden} more lines · ctrl+o to expand`}</Text>}
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

  // Your prompts: a rounded bubble on the right, like a chat app.
  on('ui.render', { component: 'UserMessage' }, ($, e, next) => {
    if (e.surface !== 'terminal' || e.props.origin.kind !== 'composer') {
      return next(e)
    }
    const { Box, Text } = $.ui.resolve(e)
    const columns = e.viewport?.columns ?? 100
    const longest = Math.max(...e.props.text.split('\n').map(line => line.length))
    const inner = Math.max(8, Math.min(longest, Math.floor(columns * 0.72) - 4))

    return (
      <Box flexDirection="row" justifyContent="flex-end" marginY={1}>
        <Box width={inner + 4} borderStyle="round" borderColor={C.border} paddingX={1}>
          <Text color={C.fg}>{e.props.text}</Text>
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
    const label = ` Worked for ${formatDuration(e.props.durationMs)} `
    const columns = e.viewport?.columns ?? 100
    const rule = '─'.repeat(Math.max(0, columns - label.length - 4))

    return (
      <Box marginTop={1}>
        <Text color={C.border}>──</Text>
        <Text color={C.muted}>{label}</Text>
        <Text color={C.border}>{rule}</Text>
      </Box>
    )
  })

  // Code blocks: a rounded card with the language as a small header.
  on('ui.render', { component: 'CodeBlock' }, ($, e, next) => {
    if (e.surface !== 'terminal' || !e.props.isClosed) {
      return next(e)
    }
    const { Box, Text, Code } = $.ui.resolve(e)
    const source = e.props.source.replace(/\n$/, '')

    return (
      <Box flexDirection="column" borderStyle="round" borderColor={C.border} paddingX={1}>
        {e.props.language !== '' && <Text color={C.teal}>{e.props.language}</Text>}
        <Code source={source} language={e.props.language || undefined} />
      </Box>
    )
  })
}
