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
        </div>
        
</apply>
