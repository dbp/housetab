<apply template="base">

  <div class="card">
    <h3>
      <a href="/sets/export">New Export</a> |
      <a href="/sets/import">New Import</a>
    </h3>

    <table>
      <tr>
        <th>Type</th>
        <th>Created</th>
        <th>Entries</th>
        <th>Download</th>
        <th>Archive/Restore</th>
      </tr>
      <sets>
        <tr>
          <td><type/></td>
          <td><created-at/></td>
          <td><count/> (<archived/> archived)</td>
          <td><a href="/sets/${id}/download/housetab-set-${created-at}.csv">link</a></td>
          <td><a href="/sets/${id}/archive">Archive</a> / <a href="/sets/${id}/restore">Restore</a></td>
        </tr>
      </sets>
    </table>
  </div>
</apply>
