<!doctype html>
<html>
  <head>
    <title>HouseTab</title>
    <link rel="stylesheet" href="/static/main.css"/>
  </head>

  <body>
    <div class="nav-holder">
      <ul class="nav">
        <isLoggedIn>
          <li class="home"><a href="/">HT :: <loggedInAccount/></a></li>
          <li><a href="/entries/add">Add Entry</a></li>
          <li><a href="/sets">Import/Export</a></li>
          <li><a href="/settings">Settings</a></li>
          <li><a href="/auth/destroy">Logout</a></li>
        </isLoggedIn>
        <notLoggedIn>
          <li class="home"><a href="/">HT :: </a></li>
          <li><a href="/auth/new">Login</a></li>
          <li><a href="/account/new">Signup</a></li>
        </notLoggedIn>
      </ul>
    </div>
    <div class="content">
      <apply-content/>
    </div>

  </body>
</html>
