<apply template="base">

  <isLoggedIn>
    <table class="people">
      <tr><td>Person</td><td>Shares</td></tr>
      <people>
        <tr>
          <td><name/><br/><a href="/settings/person/${id}/delete" onclick="return confirm('Only works if there are no entries associated with person.')">delete</a></td>
          <td>
            <shares>
              <value/> (starting <start/>) <a href="/settings/share/${id}/delete" onclick="return confirm('Are you sure?')">delete</a><br/>
            </shares>
            <form method="GET" action="/settings/share/new">
              <input type="hidden" name="person" value="${id}"/>
              <input type="text" size="5" name="value"/> (starting <input type="text" size="10" name="date" value="${now}"/>)
              <button type="submit">add</button>
            </form>
          </td>
      </people>
      <tr>
        <td colspan="2">
          <form method="GET" action="/settings/person/new">
            <label for="name">Name <input type="text" size="10" name="name"/></label>
            <button type="submit">add</button>
          </form>
        </td>
      </tr>
    </table>
  </isLoggedIn>

</apply>
