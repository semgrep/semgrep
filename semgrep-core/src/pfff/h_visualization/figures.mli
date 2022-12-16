
(* float coordinates *)
type point = { x: float; y: float }

type rectangle = { p: point; q: point; }

(* rectangles *)
val s_of_rectangle: rectangle -> string
val rect_width: rectangle -> float
val rect_height: rectangle -> float
val rect_area: rectangle -> float
val valid_rect: rectangle -> bool
val point_is_in_rectangle: point -> rectangle -> bool
val intersection_rectangles: rectangle -> rectangle -> rectangle option

(* pixel coordinates *)
type point_pixel = { xpix:int; ypix:int }
type rect_pixel = { lower_left:  point_pixel;  upper_right: point_pixel; }

(* rectangles *)
val rect_pixel_width: rect_pixel -> int
val rect_pixel_height: rect_pixel -> int
val random_point_in_rect_pixel: rect_pixel -> point_pixel
