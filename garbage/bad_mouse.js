$(document).ready(function() {
   $(document).mousemove(function(e) {
      globalMouseState.x = e.pageX
      globalMouseState.y = e.pagey
      <setButtons(e)>
   })
})
