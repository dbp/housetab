<!doctype html>
<html>
  <head>
    <title>HouseTab</title>
    <link rel="stylesheet" href="/static/main.css"/>
  </head>

  <body>
    <ul class="nav">
      <li><a href="/">HouseTab</a></li>
      <isLoggedIn>
        <li><loggedInAccount/> &mdash; <a href="/auth/destroy">Logout</a></li>
      </isLoggedIn>
      <notLoggedIn>
        <li><a href="/auth/new">Login</a></li>
        <li><a href="/account/new">Signup</a></li>
      </notLoggedIn>
    </ul>

    <div class="content">
      <apply-content/>
    </div>

  </body>
</html>
