# https://github.com/returntocorp/semgrep/issues/4965

def awesome
  something = if params[:thang]
                params[:thang]
              elsif somevar = "monkeypanda"
                somevar
              end
  #ruleid: check-symbol-dos
  something.to_sym
  return true
end
