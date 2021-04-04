exports.setRawModeImpl = (stdin, b) => {
  stdin.setRawMode(b)
  return {}
}

exports.emitKeypressEventsImpl = (stdin) => {
  const readline = require('readline')
  readline.emitKeypressEvents(stdin)
  return {}
}

exports.onKeypressImpl = (just) => (nothing) => (stdin, handler) => {
  stdin.on('keypress', (str, key) => {
    const newKey = {
      sequence: key.sequence,
      name: key.name === undefined ? nothing : just(key.name),
      ctrl: key.ctrl,
      meta: key.meta,
      shift: key.shift,
    }
    handler(str, newKey)
  })
  return {}
}

exports.removeAllListenersImpl = (stdin) => {
  stdin.removeAllListeners('keypress')
  return {}
}

exports.columnsImpl = (stdout) => {
  return stdout.columns
}
