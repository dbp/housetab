<apply template="base">

  <isLoggedIn>

    <div class="card inline">
      <h3>Emails</h3>
      <table>
        <emails>
          <tr>
            <td><email/><not-verified>(unverified)</not-verified></td>
            <td><a href="/settings/email/${id}/delete" onclick="return confirm('Are you sure?');">delete</a></td>
          </tr>
        </emails>
        <tr>
          <td colspan="2">
            <form method="GET" action="/settings/email/new">
              <input type="text" size="20" name="email"/>
              <button type="submit">add</button>
            </form>
          </td>
        </tr>
      </table>
    </div>

    <div class="card-holder">
      <people>
        <div class="card small">
          <h3><name/>'s Shares</h3>
          <h4><a href="/settings/person/${id}/delete" onclick="return confirm('Only works if there are no entries associated with person.');">delete</a></h4>

          <table>
            <shares>
              <tr><td>
                <h5><value/> on <start/> <a href="/settings/share/${id}/delete" onclick="return confirm('Are you sure?')">X</a></h5></td></tr>
            </shares>

          <tr><td><form method="GET" action="/settings/share/new">
            <input type="hidden" name="person" value="${id}"/>
            <input type="text" size="3" name="value"/> on <input type="text" size="8" name="date" value="${now}"/>
            <button type="submit">add</button>
          </form></td></tr>
          </table>
        </div>
      </people>

      <div class="card small">
        <form method="GET" action="/settings/person/new">
          <h3><label for="name">Name <input type="text" size="10" name="name"/></label>
            <button type="submit">add</button></h3>
        </form>
      </div>
    </div>
  </isLoggedIn>

</apply>
