structure Mouse = struct
	type xCoord = int
	type yCoord = int
	datatype button = LeftButton | RightButton | MiddleButton
	datatype event = ButtonUp of button | ButtonDown of button | MoveOver of xCoord * yCoord
end
open Mouse
