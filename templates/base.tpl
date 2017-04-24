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
          <li><a class="add" href="/entries/add">Add Entry</a></li>
          <li><a class="export" href="/sets">Import/Export</a></li>
          <li><a class="settings" href="/settings">Settings</a></li>
          <li><a class="logout" href="/auth/destroy">Logout</a></li>
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

    <div class="footer-holder">
      <div class="footer">
        Source at <a href="https://github.com/dbp/housetab">github.com/dbp/housetab</a> &mdash;
        Icons from the Noun Project:
        Cloud Download by Icons fest;
        add by juli;
        setting by cathy moser;
        Power by i cons.
      </div>
    </div>

  </body>
</html>
