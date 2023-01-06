
let rec factorial = function
| 0 -> 1
| n -> n * factorial (n - 1)

let rec range a b = 
  if a > b then []
  else a :: range (a + 1) b

let rec map f l = match l with
| [] -> []
| h::t -> f h :: map f t

let foreach f l = ignore (map f l)

let rec map_to_point (f: ('a -> 'b)) l = match l with
| [] -> []
| h::t -> (h, f h) :: map_to_point f t

let mandelbrot (c: Complex.t) z =
  Complex.add (Complex.mul z z) c

let mandelbrot2 max c =
  let rec aux z i = 
    if i = max || (Complex.norm z) > float_of_int max_int then z
    else aux (mandelbrot c z) (i + 1)
  in
  Complex.norm (aux Complex.zero 0)

let div_f a b = (float_of_int a) /. (float_of_int b) 
let mul_withf i f = int_of_float(float_of_int(i) *. f)

let complexPlane dimx dimy =
  let mat = Array.make_matrix dimx dimy 0 in
  let mapfunc x col = Array.mapi (fun y _ : Complex.t -> { re = (div_f (x-(dimx/2)) dimx) *. 4. -. 0.5; im = (div_f (y-(dimy/2)) dimx) *. 4.}) col in
  Array.mapi mapfunc mat

let mapArr2d f l = Array.map (Array.map f) l
let mapArr2di f l = Array.mapi (fun x col -> Array.mapi (f x) col) l

let iterArr2di f l = Array.iteri (fun x col -> Array.iteri (f x) col) l
let iterArr2d f l = Array.iter (Array.iter f) l

open Graphics

let pick_color colors v =
  let mix_rbg (r1, g1, b1) (r2, g2, b2) w =
    let mix a b = (mul_withf a w) + (mul_withf b (1.0 -. w)) in
      (mix r1 r2, mix g1 g2, mix b1 b2)
  in
  let get_rgb clr = (clr land 0xFF0000, clr land 0x00FF00, clr land 0x0000FF) in

  (* prevent overflows here *)
  if v <= 2. then black
  else let v2 = (log v) /. 10. in
    if v2 >= float_of_int (Array.length colors - 1) then colors.(Array.length colors - 1) 
    else 
    let c1 = colors.(int_of_float v2) in
    let c2 = colors.(int_of_float v2 + 1) in
    let (w, _) = Float.modf v2 in
      mix_rbg (get_rgb c1) (get_rgb c2) w |> (fun (r, g, b) -> rgb r g b) 
  
let colors = [|red|]
let pick_and_set_color v = 
 v |> pick_color colors |> set_color 

let max = 20
let dimx = 1920
let dimy = 1080

let () =
  open_graph "";
  auto_synchronize false;
  (* Thread.delay 1.; *)
  let plane = complexPlane dimx dimy in
  (* let pp = iterArr2di (fun _ _ (v: Complex.t) -> Printf.printf "%f;%f " v.re v.im) in *)
  let plane2 = mapArr2d (mandelbrot2 max) plane in
  let drawSet = iterArr2di (fun x y v -> pick_and_set_color v; plot x y) in
  drawSet plane2;
  synchronize ();
  ignore (wait_next_event [ Key_pressed ])