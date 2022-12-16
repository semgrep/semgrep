open Common

(*****************************************************************************)
(* float (vectoriel)  coordinates *)
(*****************************************************************************)
type point = { x: float; y: float }

type rectangle = { p: point; q: point; }

(*****************************************************************************)
(* Rectangles *)
(*****************************************************************************)

let rect_width r = r.q.x -. r.p.x

let rect_height r = r.q.y -. r.p.y

let rect_area rect =
  rect_width rect *. rect_height rect

let valid_rect r =
  r.q.x >= r.p.x && r.q.y >= r.p.y

let s_of_rectangle r =
  spf "p:%f, %f;  q:%f, %f" r.p.x r.p.y r.q.x r.q.y


let point_is_in_rectangle p r =
  p.x >= r.p.x && p.x <= r.q.x &&
  p.y >= r.p.y && p.y <= r.q.y



let intersection_rectangles r1 r2 =
  let r = {
    p = {
      x = max r1.p.x r2.p.x;
      y = max r1.p.y r2.p.y;
    };
    q = {
      x = min r1.q.x r2.q.x;
      y = min r1.q.y r2.q.y;
    };
  }
  in
  if valid_rect r then Some r else None

(* TODO: does not work with js_of_ocaml !!!!
   let _ = assert
   (intersection_rectangles
    { p = { x=0.; y=0.}; q = { x=4.; y=4.};}
    { p = { x=2.; y=2.}; q = { x=6.; y=6.};}
   =
    Some { p = { x=2.; y=2.}; q = { x=4.; y=4.};}
   )
*)

(*****************************************************************************)
(* pixel coordinates *)
(*****************************************************************************)
type point_pixel = { xpix:int; ypix:int }

type rect_pixel = {
  lower_left:  point_pixel;
  upper_right: point_pixel;
}

let rect_pixel_width  rect = rect.upper_right.xpix - rect.lower_left.xpix
let rect_pixel_height rect = rect.upper_right.ypix - rect.lower_left.ypix

let random_point_in_rect_pixel rect =
  let x = rect.lower_left.xpix + Random.int (rect_pixel_width rect) in
  let y = rect.lower_left.ypix + Random.int (rect_pixel_height rect) in
  { xpix = x; ypix = y }
