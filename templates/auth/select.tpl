    <apply template="base">
        <div class="card">
            <h3>Login</h3>
            <br/>

            <form method="POST">
                <input type="hidden" name="account" value="${account}"/>
                <label>Email
                    <select name="email">
                        <emails>
                            <option value="${id}"><email/></option>
                        </emails>
      </select>
    </label>
    <button type="submit">Submit</button>
            </form>

            <p>
Please select an email address for us to send a secure one-time login link to. Once you've logged in, you can add more email addresses to your account. Note that these email addresses are somewhat obscured for security.
            </p>
        </div>

</apply>
