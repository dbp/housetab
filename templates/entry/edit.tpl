<apply template="base">
  <dfForm>
    <dfChildErrorList ref=""/>
    <dfLabel ref="date">
      Date
      <dfInput ref="date"/>
    </dfLabel>

    <dfLabel ref="description">
      Description
      <dfInput ref="description"/>
    </dfLabel>

    <dfLabel ref="howmuch">
      How Much
      <dfInput ref="howmuch"/>
    </dfLabel>

    <dfLabel ref="who">
      Who
      <dfInputSelect ref="who"/>
    </dfLabel>

    <dfLabel ref="whopays">
      Whopays
      <dfList ref="whopays">
        <dfLabel ref="present">
          <dfInputCheckbox ref="present"/>
          <dfPlainText ref="name" />
        </dfLabel>
      </dfList>
    </dfLabel>


    <dfInputSubmit/>
  </dfForm>
</apply>
