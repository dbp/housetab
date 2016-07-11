<apply template="base">

  <form method="POST">
    <input type="hidden" name="account" value="${account}"/>
    <label>Email
      <select name="email">
        <emails>
          <option value="${id}"><email/></option>
        </emails>
      </select>
    </label>
    <button type="submit"/>
  </form>

</apply>
