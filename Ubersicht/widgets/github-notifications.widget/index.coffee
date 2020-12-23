user          = 'babariviere'
apiKey        = '58d71f547a51a7a134a0996b5feb036ef5be0025'
publicApi     = 'https://api.github.com'
enterpriseApi = '<your github enterprise api url here>'
enterprise    = false
proxyUrl      = '<your https proxy url string here>'
proxy         = false

api = if enterprise then enterpriseApi else publicApi

proxyCmd = if proxy then "-x #{proxyUrl}" else ""

cmd = "curl -s --user #{user}:#{apiKey} -s #{api}/notifications #{proxyCmd}"

command: cmd

enterprise: enterprise

refreshFrequency: 15000

style: """
  position: absolute;
  right: 19%;

  a:link, a:visited, a:hover, a:active {
    color: #000
  }

  .github-notifications {
    border-radius: 30px;
    margin-top: 5px;
    margin-bottom: 5px;
    padding-top: 5px;
    padding-bottom: 5px;
    height: 20px;
    background-color: #282a36;
  }

  .enterprise {
    color: #E5E5E5;
    background-color: #2A2C2E;
  }

  @font-face {
    font-family: 'octicons';
    src: url('github-notifications.widget/octicons/octicons.eot?#iefix') format('embedded-opentype'),
         url('github-notifications.widget/octicons/octicons.woff') format('woff'),
         url('github-notifications.widget/octicons/octicons.ttf') format('truetype'),
         url('github-notifications.widget/octicons/octicons.svg#octicons') format('svg');
    font-weight: normal;
    font-style: normal;
  }

  .octicon {
    font: normal normal normal 14px/1 octicons;
    color: #f8f8f2;
    display: inline-block;
    text-decoration: none;
    text-rendering: auto;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    -webkit-user-select: none;
    -moz-user-select: none;
    -ms-user-select: none;
    user-select: none;
  }

  sub {
    line-height: 0;
    bottom: -0.25em;
  }

  .count-group {
    display: inline-block;
    width: 30px;
    font-size: 8px;
    text-align: center;
  }

  .count-group + .count-group {
    padding-left: 5px;
    border-left: 1px solid #CCC;
  }

  .count {
    font-size: 6px;
    background-color: #ff5555;
    border-radius: 10px;
    padding: 3px 5px;
    margin-left: -5px;
  }

  .public .count {
    color: #FFF;
  }

"""

iconMapping: [
  ["mention"     ,"&#xf0be;"],
  ["author"      ,"&#xf018;"],
  ["team_mention","&#xf037;"],
  ["assign"      ,"&#xf035;"]
  ["manual"      ,"&#xf077;"],
  ["comment"     ,"&#xf02b;"],
  ["subscribed"  ,"&#xf04e;"],
  ["state_change","&#xf0ac;"],
]

render: (output) -> @getVisual output

update: (output, domEl) ->
  $domEl = $(domEl)
  $domEl.html @getVisual output

getVisual: (output) ->
  data = []
  try
    data = JSON.parse output
  catch ex
    return """
      <div class='github-notifications #{if @enterprise then "enterprise" else "public"}'>
        <span>Error Retrieving Notifications!</span>
      </div>
    """

  counts =
    subscribed: 0
    manual: 0
    author: 0
    comment: 0
    mention: 0
    team_mention: 0
    state_change: 0
    assign: 0

  for reason in (notification.reason for notification in data)
    counts[reason] += 1

  icons = []

  for icon in @iconMapping
    count =
      if counts[icon[0]] > 0
        count = "<sub><span class='count'>#{counts[icon[0]]}</span></sub>"
      else
        ''
    icons.push "<div class='count-group'><span class='octicon'>#{icon[1]}</span>#{count}</div>"

  return """
    <a href='https://www.github.com/' + @user'>
      <div class='github-notifications #{if @enterprise then "enterprise" else "public"}'>
        #{icons.join('')}
      </div>
    </a>
  """