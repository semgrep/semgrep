
func (c *UserController) Login(w http.ResponseWriter, r *http.Request, ctx *rack.Context) {
     w.Header().Set("Access-Control-Allow-Origin", r.Header.Get("Origin"))
     w.Header().Set("Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT, DELETE")
     c.render.Json(w,rsp, http.StatusOK)
     //ERROR: match
     c := cors.New(cors.Options{ AllowedOrigins: []string{"*"}, })
     return
}
