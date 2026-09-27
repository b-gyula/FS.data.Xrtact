<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" >
	<xsl:output method="html" indent="no" media-type="text/json"/>
	<xsl:template match="/">
{	"crop": {<xsl:apply-templates select="fs/crop"/>
	},<xsl:apply-templates select="fs/price"/>,
	"factory": {<xsl:apply-templates select="fs/factory"/>
	}
}
	</xsl:template>

	<xsl:template match="fs/crop">
		<xsl:for-each select="*">
		"<xsl:value-of select="name()"/>": {<xsl:if test="@seed">
			"seed": <xsl:value-of select="@seed"/>,</xsl:if><xsl:if test="@windrow">
			"windrow": <xsl:value-of select="@windrow"/>,</xsl:if>
			"yield": <xsl:value-of select="@yield"/>
			}<xsl:if test="position()!=last()">,</xsl:if>
		</xsl:for-each>
	</xsl:template>

	<xsl:template match="fs/price">
	"price": {<xsl:call-template name="copyAttribs"/>
	}
	</xsl:template>

	<xsl:template match="fs/factory">
		<xsl:for-each select="*">
		"<xsl:value-of select="name()"/>": {<xsl:if test="@variant">
			"variant":"<xsl:value-of select="@variant"/>",</xsl:if>
			"price": <xsl:value-of select="@price"/>,
			"recipe": {<xsl:for-each select="*">
				<xsl:call-template name="copy"/>
			<xsl:if test="position()!=last()">,</xsl:if>
			</xsl:for-each>}
			}<xsl:if test="position()!=last()">,</xsl:if>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="copyAttribs">
		<xsl:for-each select="@*">
		"<xsl:value-of select="name()"/>": <xsl:value-of select="."/>
		<xsl:if test="position()!=last()">,</xsl:if>
		</xsl:for-each>
	</xsl:template>

	<xsl:template name="copy">
	"<xsl:value-of select="name()"/>": {
		<xsl:call-template name="copyAttribs"/><xsl:if test="*">,
		<xsl:for-each select="*">
			<xsl:call-template name="copy"/>
			<xsl:if test="position()!=last()">,</xsl:if>
		</xsl:for-each></xsl:if>
		}
	</xsl:template>

</xsl:stylesheet>