<!doctype html>
<html>
  <head>
    <title>HouseTab</title>
    <link rel="stylesheet" href="/static/main.css"/>
  </head>

  <body>
      <div id="banner" style="background-color: #FFBA00; padding: 10px;">
          <div style="max-width: 800px; margin: 0 auto; text-align: center;">
              <h2 style="font-weight: bold;">HouseTab has changed!</h2>

              <p>
                  You're looking at a totally new version of HouseTab!
                  <strong>Important details:</strong>
              </p>

              <ul style="text-align: left">
                  <li>All your data is still here, and all core editing functionality remains.</li>
                  <li><strong>Logins are now by email</strong> - to log in, you
                      select an email on the account and we email a link to click.
                      Your email has been added, but once you log in you can add others.</li>
                  <li><strong>Exporting and importing</strong> data from CSVs is now supported, for backups and migrating to/from other tools.</li>
                  <li>History and the tutorial have been removed.</li>
                  <li>Now more mobile-friendly (with a totally different design).</li>
              </ul>

              &darr; <a href="#" onclick="document.getElementById('readmore').setAttribute('style','text-align: left')">Read More</a>  &darr;

              <ul id="readmore" style="display: none">
                  <li>HouseTab was written in 2011, and has run, unchanged,
                      since then. It was build and then hosted since then (making no money) by
                      <a href="http://positionstudios.com/">Position Studion</a>,
                      and it is <strong>a bit dated</strong>. </li>
                  <li>All the software it uses is also old, so making changes is
                      quite difficult - in particular, <strong>we couldn't move it to more modern hosting!</strong></li>
                  <li>The database that it uses (MongoDB) is unecessarily buggy, which has
                      increased the work that we've had to do in order to keep
                      everything working.</li>
                  <li>In mid-2016, I started <strong>rewriting it from scratch</strong>, with
                      forwards mantainability in mind, and backed by a well-understood and trusted database (PostgreSQL).
                      We also added a longstanding feature request: exporting data.
                  </li>
                  <li>Another change in the rewrite was to no longer use
                      passwords. Migrating encrypted passwords is difficult in general (because
                      you have to continue to use whatever encryption scheme was used),
                      and while it was possible, the rewrite instead relies
                      instead on <strong>one-time links that are sent our when users
                      want to log in</strong>.</li>
                  <li>It's still completely <strong>open source</strong>: <a href="https://github.com/dbp/housetab">github.com/dbp/housetab</a>,
                      still <strong>completely free</strong>! The rewrite was primarily done to make it easier to mantain.</li>
              </ul>

          </div>
      </div>

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
