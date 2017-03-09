<apply template="base">
<div class="card">
    <h3>Signup for an account</h3>
    <dfForm>
        <table>
            <tr>
                <td colspan="2">
                    <dfChildErrorList ref=""/>
                </td>
            </tr>
            <tr>
                <td>
                    <dfLabel ref="account">
                        Account Name
                    </dfLabel>
                </td>
                <td>
                    <dfInput ref="account"/>
                </td>
            </tr>

            <tr>
                <td>
                    <dfLabel ref="email">
                        Email for Login (more can be added)
                    </dfLabel>
                </td>
                <td>
                    <dfInput ref="email"/>
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

