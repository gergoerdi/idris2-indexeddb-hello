module Utils

import JS

%foreign "browser:lambda:(_a, _b, x, y) => ((console.log(x),y))"
export traceConsole : a -> b -> b

export
traceConsoleId : a -> a
traceConsoleId x = traceConsole x x
