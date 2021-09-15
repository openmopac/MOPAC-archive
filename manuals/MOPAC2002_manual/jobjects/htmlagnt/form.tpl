<body bgcolor="#FFFFFF">
<form name="simpleform" onSubmit="parent.qaframe.PerformSearch( this ); return false;">
  <table border="0" width="570" align="center" bgcolor="#e4e4e4" bordercolor="#c0c0c0" bordercolorlight="#c0c0c0" bordercolordark="#c0c0c0">
    <tr> 
      <td valign="top" colspan="2" bgcolor="#000000"> 
        <div align="center"><font color="#FFFFFF" size="+1">Content Search</font></div>
      </td>
    </tr>
<!--    <tr>
      <td valign="top"><b>Search In</b><font size="-1"><br>
        Select content domain for your search.</font></td>
      <td valign="top">
        <select name="collection">
          <option value=1 %rt.collection1.selected%>Collection Title
        </select>
      </td>
    </tr>
 -->
    <tr> 
      <td valign="top"> <b>Search For</b><font size="-1"><br>
        Enter keywords or boolean expression for your search. The boolean expression 
        allows "and", "or", &quot;not&quot; and parenthesis.</font></td>
      <td valign="top"> 
        <input type="text" name="query" size="25" value="%rt.previous_query%">
        <input type="button" name="submit" value="Search"
               onClick="parent.qaframe.PerformSearch( this.form )" >
      </td>
    </tr>
    <tr> 
      <td valign="top"> <b>Return Number </b><font size="-1"><br>
        Return how many documents?</font></td>
      <td valign="top"> 
        <select name="maxhits">
          <option value="200" %rt.max_200%>200</option>
          <option value="100" %rt.max_100%>100</option>
          <option value="50" %rt.max_50%>50</option>
          <option value="25" %rt.max_25%>25</option>
        </select>
      </td>
    </tr>
    <tr> 
      <td valign="top"> <b>Return Format </b><font size="-1"><br>
        "Full descriptions" includes a summary, "Title only" does not</font>.</td>
      <td valign="top"> 
        <select name="template_html_hits">
          <option value="htmlagnt/hits.tpl" %rt.result_0%>Full descriptions</option>
          <option value="htmlagnt/hitsnod.tpl" %rt.result_1%>Title only</option>
        </select>
      </td>
    </tr>
    <tr> 
      <td valign="top"><b>Keyword Highlighting</b><br>
        <font size="-1">If selected, keywords will be emphasized in found documents.</font></td>
      <td valign="top">
        <input type="checkbox" name="show_highlighted" %rt.show_hl%>
        Highlight keywords</td>
    </tr>
    <tr>
      <td valign="top"><b>Navigation Controls</b><br>
        <font size="-1">If selected, found documents will contain special controls 
        for navigation among keywords. Useful for finding words in long documents.</font></td>
      <td valign="top">
        <input type="checkbox" name="UseNavigationButtons" %rt.use_navigation%>
        Show navigation controls</td>
    </tr>
  </table>
</form>
