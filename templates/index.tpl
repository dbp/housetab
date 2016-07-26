<apply template="base">

  <isLoggedIn>
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
            <a href="/entries/${id}/delete">delete</a>
          <td>
            <date/>
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
