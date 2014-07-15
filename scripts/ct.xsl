<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<xsl:param name="vendor" select="'NIST'"/>
    <xsl:output method="text"/>   

    <xsl:template match="/">
      <xsl:apply-templates />
    </xsl:template>
  

  <xsl:template match="div">
    <xsl:choose>
      <xsl:when test="@class='B'">\chapter{</xsl:when>
      <xsl:when test="@class='D'">\section{</xsl:when> 
      <!--<xsl:when test="@class='D'">\chapter{</xsl:when>
      <xsl:when test="@class='G'">\section{</xsl:when> 
      <xsl:when test="@class='E'">\section{</xsl:when>
      <xsl:when test="@class='G'">\subsection{</xsl:when>
      <xsl:when test="@class='E'">\subsection{</xsl:when>-->
    </xsl:choose><xsl:value-of select="."/>
    <xsl:if test="following::div[@class='cuatro' and position() = 1]/text() != ''"> - <xsl:copy-of select="following::div[@class='cuatro' and position()=1]/text()" /></xsl:if>
    <xsl:if test="@class='B' or @class='C' or @class='D' or @class='G' or @class='E'">}</xsl:if>
  </xsl:template>

   

   

  <xsl:template match="p">
   <xsl:value-of select="." />
   <xsl:text>
   </xsl:text>
  </xsl:template>
  
 <xsl:template match="div[@class='centro']|div[@class='uno']|div[@class='dos']|div[@class='tres']"></xsl:template>

</xsl:stylesheet>
