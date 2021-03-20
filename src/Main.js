exports.setRawModeImpl = (stdin, b) => {
  stdin.setRawMode(b)
  return {}
}

exports.emitKeypressEventsImpl = (stdin) => {
  const readline = require('readline')
  readline.emitKeypressEvents(stdin)
  return {}
}

exports.onKeypressImpl = (stdin, handler) => {
  stdin.on('keypress', handler)
  return {}
}

exports.removeAllListenersImpl = (stdin) => {
  stdin.removeAllListeners('keypress')
  return {}
}
