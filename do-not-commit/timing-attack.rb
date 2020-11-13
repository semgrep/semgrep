class timing_attack
  # ruleid: timing-attack
  http_basic_authenticate_with name: "Chris", password: "LimpBizkitRules420"
  # ruleid: timing-attack
  http_basic_authenticate_with :name => ENV["NAME"], :password => ENV["PASSWORD"]
end
