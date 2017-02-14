<apply template="base">
  <div class="card">
    <h3>Entry</h3>
    <dfForm>
      <table>
        <tr>
          <td colspan="2">
            <dfChildErrorList ref=""/>
          </td>
        </tr>
        <tr>
          <td>
            <dfLabel ref="date">
              Date
            </dfLabel>
          </td>
          <td>
            <dfInput ref="date"/>
          </td>
        </tr>

        <tr>
          <td>
            <dfLabel ref="description">
              Description
            </dfLabel>
          </td>
          <td>
            <dfInput ref="description"/>
          </td>
        </tr>

        <tr>
          <td>
            <dfLabel ref="howmuch">
              How Much
            </dfLabel>
          </td>
          <td>
            <dfInput ref="howmuch"/>
          </td>
        </tr>

        <tr>
          <td>
            <dfLabel ref="who">
              Who
            </dfLabel>
          </td>
          <td>
            <dfInputSelect ref="who"/>
          </td>
        </tr>

        <tr>
          <td>
            <dfLabel ref="whopays">
              Who Pays
            </dfLabel>
          </td>
          <td>
            <dfList ref="whopays">
              <dfLabel ref="present">
                <dfInputCheckbox ref="present"/>
                <dfPlainText ref="name" />
              </dfLabel>
              <br/>
            </dfList>
          </td>
        </tr>
        <tr>
          <td>
            <a href="/">Cancel</a>
          </td>
          <td>
            <dfInputSubmit/>
          </td>
        </tr>
      </table>
    </dfForm>
  </div>
</apply>
