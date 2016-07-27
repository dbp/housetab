<apply template="base">

  <isLoggedIn>
    <table class="results">
      <tr>
        <th>
          Person
        </th>
        <th>
          Spent
        </th>
        <th>
          Owes
        </th>
      </tr>
      <results>
        <tr>
          <td>
            <person><name/></person>
          </td>
          <td>
            <spent/>
          </td>
          <td>
            <owes/>
          </td>
        </tr>
      </results>
    </table>

    <table class="entries">
      <tr>
        <th></th>
        <th>Date</th>
        <th>Description</th>
        <th>By</th>
        <th>For</th>
        <th>Amount</th>
      </tr>
      <entries>
        <tr>
          <td>
            <a href="/entries/${id}/edit">edit</a> &ndash;
            <a href="/entries/${id}/delete" onclick="return confirm('are you sure?')">delete</a>
          <td>
            <span style="white-space: nowrap"><date/></span>
          </td>
          <td>
            <description/>
          </td>
          <td>
            <who/>
          </td>
          <td>
            <whopays>
              <name/><sep/>
            </whopays>
          </td>
          <td>
            <howmuch/>
          </td>
        </tr>
      </entries>
    </table>
  </isLoggedIn>

  <notLoggedIn>
    About HouseTab
  </notLoggedIn>

</apply>
