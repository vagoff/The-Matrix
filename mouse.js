var globalMouseState = {};

function getCurrentMouseX() { return globalMouseState.x }
function getCurrentMouseY() { return globalMouseState.y }
function getCurrentMouseButtons() { return globalMouseState.buttons }

$(document).ready(function() {
   $(document).mousemove(function(e) {
      globalMouseState.x = e.pageX
      globalMouseState.y = e.pagey
      globalMouseState.buttons = e.which
   })
})
