<apply template="base">

  <h1>HouseTab [<loggedInAccount/>]</h1>

  <script type="text/javascript">
   // From http://stackoverflow.com/questions/2044616/select-a-complete-table-with-javascript-to-be-copied-to-clipboard
   function selectElementContents(el) {
     var body = document.body, range, sel;
     if (document.createRange && window.getSelection) {
       range = document.createRange();
       sel = window.getSelection();
       sel.removeAllRanges();
       try {
         range.selectNodeContents(el);
         sel.addRange(range);
       } catch (e) {
         range.selectNode(el);
         sel.addRange(range);
       }
     } else if (body.createTextRange) {
       range = body.createTextRange();
       range.moveToElementText(el);
       range.select();
     }
   }
  </script>

  <button onclick="selectElementContents(document.getElementById('entries'))">Select For Copying</button>

  <table id="entries">
    <tr>
      <th>Date</th>
      <th>Description</th>
      <th>By</th>
      <th>For</th>
      <th>Amount</th>
    </tr>
    <entries>
      <tr>
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

</apply>
