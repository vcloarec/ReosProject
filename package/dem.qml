<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis hasScaleBasedVisibilityFlag="0" styleCategories="AllStyleCategories" minScale="1e+08" maxScale="0" version="3.18.1-Zürich">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal enabled="0" fetchMode="0" mode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <property value="false" key="WMSBackgroundLayer"/>
    <property value="false" key="WMSPublishDataSourceUrl"/>
    <property value="0" key="embeddedWidgets/count"/>
    <property value="Value" key="identify/format"/>
  </customproperties>
  <pipe>
    <provider>
      <resampling zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" enabled="false" maxOversampling="2"/>
    </provider>
    <rasterrenderer classificationMax="1332.2408447" band="1" alphaBand="-1" classificationMin="-0.5396354" type="singlebandpseudocolor" nodataColor="" opacity="1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>MinMax</limits>
        <extent>UpdatedCanvas</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <rastershader>
        <colorrampshader classificationMode="1" colorRampType="INTERPOLATED" clip="0" labelPrecision="4" maximumValue="1332.2408447" minimumValue="-0.5396354">
          <colorramp name="[source]" type="gradient">
            <Option type="Map">
              <Option value="166,97,26,255" name="color1" type="QString"/>
              <Option value="1,133,113,255" name="color2" type="QString"/>
              <Option value="0" name="discrete" type="QString"/>
              <Option value="gradient" name="rampType" type="QString"/>
              <Option value="0.25;223,194,125,255:0.5;245,245,245,255:0.75;128,205,193,255" name="stops" type="QString"/>
            </Option>
            <prop k="color1" v="166,97,26,255"/>
            <prop k="color2" v="1,133,113,255"/>
            <prop k="discrete" v="0"/>
            <prop k="rampType" v="gradient"/>
            <prop k="stops" v="0.25;223,194,125,255:0.5;245,245,245,255:0.75;128,205,193,255"/>
          </colorramp>
          <item label="-0.5396" value="-0.539635419845581" color="#a6611a" alpha="255"/>
          <item label="332.7" value="332.65548461675644" color="#dfc27d" alpha="255"/>
          <item label="665.9" value="665.8506046533585" color="#f5f5f5" alpha="255"/>
          <item label="999" value="999.0457246899605" color="#80cdc1" alpha="255"/>
          <item label="1332" value="1332.2408447265625" color="#018571" alpha="255"/>
          <rampLegendSettings prefix="" direction="0" useContinuousLegend="1" suffix="" orientation="2" maximumLabel="" minimumLabel="">
            <numericFormat id="basic">
              <Option type="Map">
                <Option value="" name="decimal_separator" type="QChar"/>
                <Option value="6" name="decimals" type="int"/>
                <Option value="0" name="rounding_type" type="int"/>
                <Option value="false" name="show_plus" type="bool"/>
                <Option value="true" name="show_thousand_separator" type="bool"/>
                <Option value="false" name="show_trailing_zeros" type="bool"/>
                <Option value="" name="thousand_separator" type="QChar"/>
              </Option>
            </numericFormat>
          </rampLegendSettings>
        </colorrampshader>
      </rastershader>
    </rasterrenderer>
    <brightnesscontrast gamma="1" contrast="0" brightness="0"/>
    <huesaturation colorizeOn="0" colorizeRed="255" colorizeGreen="128" colorizeBlue="128" saturation="0" grayscaleMode="0" colorizeStrength="100"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
