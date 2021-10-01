{ writeScriptBin, gnupg, gawk, jq }:

writeScriptBin "gitlab-ci-lint" ''
  #!/bin/sh

  token=$(${gnupg}/bin/gpg --decrypt ~/.authinfo.gpg 2>/dev/null | ${gawk}/bin/awk '/gitlab.com\/api\/v4/{print $6}')
  content=$(${jq}/bin/jq -n --rawfile content .gitlab-ci.yml '. | .content=$content')
  curl -s -H "Content-Type: application/json" -H "PRIVATE-TOKEN: ''${token}" \
  	"https://gitlab.com/api/v4/ci/lint" --data "''${content}" | ${jq}/bin/jq
''
