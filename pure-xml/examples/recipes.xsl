<?xml version="1.0"?>

<xsl:stylesheet version="1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
   <xsl:apply-templates />
</xsl:template>

<xsl:template match="list">
  <html>
  <head>
    <title>My Mom's Recipes</title>
  </head>
  <body>

  <h2>My Mom's Recipes</h2>

  <hr/>		

  <xsl:apply-templates />

  </body>
  </html>
</xsl:template>

<xsl:template match="recipe">

  <table>
    <tr><td><b>Recipe:</b></td>
        <td><xsl:value-of select="recipe_name" /></td>
    </tr>
    <tr><td><b>Category:</b></td>
        <td><em><xsl:value-of select="meal" />/<xsl:value-of select="course" /></em></td>
    </tr>
    <tr><td><b>Author:</b></td>
        <td><xsl:value-of select="author" /></td>
    </tr>
  </table>

  <p><b>Ingredients:</b>
  <ul>
    <xsl:for-each select="ingredients/item">
      <li><xsl:value-of select="."/></li>
    </xsl:for-each>
  </ul></p>

  <p><b>Directions:</b> <xsl:value-of select="directions"/></p>
  <hr/>		

</xsl:template>

</xsl:stylesheet>
