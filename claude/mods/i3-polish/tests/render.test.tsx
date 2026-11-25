import { expect, test } from 'claude-code/testing'

const base = { plugin: 'i3-polish', surface: 'terminal' } as const
const patch = [{ oldStart: 3, oldLines: 2, newStart: 3, newLines: 3, lines: [' keep', '-old', '+new', '+more'] }]

test('an edit draws a card with the file name and counts', async $ => {
  const ui = await $.ui.mount({
    ...base, component: 'ToolUse',
    props: {
      tool_use_id: 't1', tool: 'Edit', input: {}, isRunning: false, isErrored: false, isInterrupted: false, isExpanded: false,
      output: { filePath: '/home/user/dev/app/src/main.ts', oldString: 'old', newString: 'new\nmore', originalFile: null,
        structuredPatch: patch, userModified: false, replaceAll: false },
    },
  })
  expect(await ui.find({ type: 'Code' })).toBeDefined()
  expect(await ui.find({ type: 'Text', text: 'main.ts' })).toBeDefined()
  expect(await ui.find({ type: 'Text', text: '+2' })).toBeDefined()
  expect(await ui.find({ type: 'Text', text: ' -1' })).toBeDefined()
  await ui.unmount()
})

test('a prompt draws as a bubble, a turn as a divider', async $ => {
  const msg = await $.ui.mount({ ...base, component: 'UserMessage', props: { text: 'hello there', origin: { kind: 'composer' }, isExpanded: false } })
  expect(await msg.find({ type: 'Text', text: 'hello there' })).toBeDefined()
  await msg.unmount()
  const turn = await $.ui.mount({ ...base, component: 'TurnDuration', props: { word: 'Baked', durationMs: 130000, isExpanded: false } })
  expect(await turn.find({ type: 'Text', text: ' Worked for 2m 10s ' })).toBeDefined()
  await turn.unmount()
})
