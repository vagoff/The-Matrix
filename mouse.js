$(document).ready(function() {
    var globalMouseState = { x: 0, y: 0, buttons: 0 };

    $(document).mousecapture({
	down: function(e, s) {
	    globalMouseState.x = e.pageX
	    globalMouseState.y = e.pageY
            if (e.which == 1) /* LMB */
		globalMouseState.buttons |= 1
	    if (e.which == 2) /* MMB */
		globalMouseState.buttons |= 2
	    if (e.which == 3) /* RMB */
		globalMouseState.buttons |= 3
	},
	move: function(e, s) {
	    globalMouseState.x = e.pageX
	    globalMouseState.y = e.pageY
	},
	up: function(e, s) {
	    globalMouseState.x = e.pageX
	    globalMouseState.y = e.pageY
            if (e.which == 1) /* LMB */
		globalMouseState.buttons &= ~1
	    if (e.which == 2) /* MMB */
		globalMouseState.buttons &= ~2
	    if (e.which == 3) /* RMB */
		globalMouseState.buttons &= ~4
	}})

    window.getCurrentMouseX = function () { return globalMouseState.x }
    window.getCurrentMouseY = function () { return globalMouseState.y }
    window.getCurrentMouseButtons = function() { return globalMouseState.buttons }
})
