<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>

  <xsl:param name="html.stylesheet">/support/styles.css</xsl:param>
  <xsl:param name="toc.section.depth">3</xsl:param>
  <xsl:param name="annotate.toc">0</xsl:param>

  <xsl:param name="generate.id.attributes" select="1"></xsl:param>
  <xsl:param name="header.rule" select="0"></xsl:param>
  <xsl:param name="footer.rule" select="0"></xsl:param>
  <xsl:param name="html.cleanup" select="1"></xsl:param>
  <xsl:param name="admon.style"><xsl:text></xsl:text></xsl:param>
  <xsl:param name="admon.graphics" select="1"></xsl:param>
  <xsl:param name="admon.graphics.path">/support/figs/</xsl:param>

  <xsl:template match="sect1" mode="toc">
    <xsl:param name="toc-context" select="."/>
    <xsl:call-template name="subtoc">
      <xsl:with-param name="toc-context" select="$toc-context"/>
      <xsl:with-param name="nodes" 
        select="sect2|refentry|bridgehead[$bridgehead.in.toc != 0]"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template match="sect2" mode="toc">
    <xsl:param name="toc-context" select="."/>

    <xsl:call-template name="subtoc">
      <xsl:with-param name="toc-context" select="$toc-context"/>
      <xsl:with-param name="nodes" 
        select="sect3|refentry|bridgehead[$bridgehead.in.toc != 0]"/>
    </xsl:call-template>
  </xsl:template>

  <!-- Add id attributes to <p> tags. This is mostly a copy of the
       base XSL. -->
  <xsl:template name="paragraph">
    <xsl:param name="class" select="''"/>
    <xsl:param name="content"/>

    <xsl:variable name="p">
      <p>
        <xsl:call-template name="dir"/>
        <xsl:if test="$class != ''">
          <xsl:apply-templates select="." mode="class.attribute">
            <xsl:with-param name="class" select="$class"/>
          </xsl:apply-templates>
        </xsl:if>
        <!-- Here we go. -->
        <xsl:if test="$generate.id.attributes != 0">
          <xsl:attribute name="id">
            <xsl:call-template name="object.id"/>
          </xsl:attribute>
        </xsl:if>
        <xsl:copy-of select="$content"/>
      </p>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$html.cleanup != 0">
        <xsl:call-template name="unwrap.p">
          <xsl:with-param name="p" select="$p"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$p"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Add id attributes to <programlisting> and <screen> tags. Once
       again, this is mostly a copy of the base XSL, although rather
       trimmed down. -->
  <xsl:template match="programlisting|screen">
    <xsl:param name="suppress-numbers" select="'0'"/>

    <xsl:call-template name="anchor"/>

    <pre>
      <!-- Here we go. -->
      <xsl:if test="$generate.id.attributes != 0">
        <xsl:attribute name="id">
          <xsl:call-template name="object.id"/>
        </xsl:attribute>
      </xsl:if>

      <xsl:apply-templates select="." mode="class.attribute"/>
      <xsl:call-template name="apply-highlighting"/>
    </pre>
  </xsl:template>

  <!-- The default stylesheet generates a little TOC at the beginning
       of each qandaset.  Uh, no thanks. -->
  <xsl:template name="process.qanda.toc"/>

  <xsl:template name="user.head.content">
    <link rel="alternate" type="application/atom+xml" title="Comments"
      href="/feeds/comments/"/>
    <script type="text/javascript" src="/support/jquery-min.js"></script>
    <script type="text/javascript" src="/support/form.js"></script>
    <script type="text/javascript" src="/support/hsbook.js"></script>
  </xsl:template>

  <xsl:template name="user.footer.content">
    <div class="rwhfooter">
      <p><img src="/support/figs/rss.png"/> Want to stay up to date? Subscribe to the comment feed for <a id="chapterfeed" class="feed" href="/feeds/comments/">this chapter</a>, or the <a class="feed" href="/feeds/comments/">entire book</a>.</p>
      <p>Copyright 2007 Bryan O'Sullivan, Don Stewart, and
      John Goerzen. This work is licensed under a <a rel="license"
      href="http://creativecommons.org/licenses/by-nc/3.0/">Creative
      Commons Attribution-Noncommercial 3.0 License</a>. Icons by <a href="mailto:mattahan@gmail.com">Paul Davey</a> aka <a href="http://mattahan.deviantart.com/">Mattahan</a>.</p>
    </div>
  </xsl:template>

  <xsl:template name="user.footer.navigation">
    <script src="http://www.google-analytics.com/urchin.js" type="text/javascript"></script>
    <script type="text/javascript">_uacct = "UA-1805907-3"; urchinTracker();</script>
  </xsl:template>
</xsl:stylesheet>
