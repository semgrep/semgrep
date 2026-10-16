c = "%{ for k, v in items }%{ if v.enabled }${v.name}%{ else }none%{ endif }%{ endfor }"
d = <<EOF2
%{ for v in items ~}
${v}
%{ endfor ~}
EOF2
