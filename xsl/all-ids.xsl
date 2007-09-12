<?xml version="1.0" encoding="utf-8"?>

<!-- Prepare an ASCII dump file of all IDs, and the pages in which
     they live, for loading into a database. Assumes one-level chunked
     HTML output, with each chunk containing either a chapter or
     sect1. -->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <xsl:output method="text"/>
  <xsl:strip-space elements="title"/>

  <xsl:template match="/">
    <xsl:for-each select="//chapter|//appendix|//sect1">
      <xsl:variable name="id">
        <xsl:value-of select="@id"/>
      </xsl:variable>
      <xsl:variable name="sectitle">
        <xsl:value-of select="normalize-space(./title)"/>
      </xsl:variable>
      <xsl:for-each select=".//para[@id]">
        <xsl:value-of select="@id"/>
        <xsl:text>|</xsl:text>
        <xsl:copy-of select="$id"/>
        <xsl:text>|</xsl:text>
        <xsl:copy-of select="$sectitle"/>
        <xsl:text>|</xsl:text>
        <xsl:copy-of select="$chaptitle"/>
        <xsl:text>&#x0a;</xsl:text>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
