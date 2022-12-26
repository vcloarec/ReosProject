/***************************************************************************
  reosgriddedrainfallrenderer_p.h - ReosGriddedRainfallRenderer_p

 ---------------------
 begin                : 16.11.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSGRIDDEDRAINFALLRENDERER_P_H
#define REOSGRIDDEDRAINFALLRENDERER_P_H

#include <qgsrasterdataprovider.h>
#include <qgsprovidermetadata.h>

#include "reosgriddedrainitem.h"
#include "reosrenderersettings_p.h"

class QgsRasterLayer;
class ReosGriddedRainfallRasterProvider_p;
class ReosGriddedRainfallColorShaderSettings_p;

class ReosRendererGriddedRainfallMapTimeStamp_p: public ReosRendererObjectMapTimeStamp
{
  public:
    ReosRendererGriddedRainfallMapTimeStamp_p( int index );
    bool equal( ReosRendererObjectMapTimeStamp *other ) override;

  private:
    int mDataIndex = -1;
};


class ReosGriddedRainfallRendererFactory_p : public ReosGriddedRainfallRendererFactory
{
  public:
    ReosGriddedRainfallRendererFactory_p( ReosGriddedRainfall *rainfall );
    ReosGriddedRainfallRendererFactory_p( const ReosEncodedElement &element, ReosGriddedRainfall *rainfall );

    ReosObjectRenderer *createRasterRenderer( ReosRendererSettings *settings ) override;
    ReosColorShaderSettings *colorRampShaderSettings() const override;
    ReosEncodedElement encode() const override;

    QgsColorRampShader colorRampShader() const;

    void setColorRampShader( const QgsColorRampShader &colorRampShader );

  private:
    std::unique_ptr<QgsRasterLayer> mRasterLayer;
    QPointer<ReosGriddedRainfallRasterProvider_p> mDataProvider;
    std::unique_ptr<ReosGriddedRainfallColorShaderSettings_p> mColorRampSettings;

    friend class  ReosGriddedRainfallColorShaderSettings_p;

    void init();
};

class ReosGriddedRainfallColorShaderSettings_p : public ReosColorShaderSettings_p
{
  public:
    ReosGriddedRainfallColorShaderSettings_p( ReosGriddedRainfallRendererFactory_p *rendererFactory );

    bool isValid() const override;
    double classificationMinimum() const override;
    void setClassificationMinimum( double newClassificationMinimum ) override;
    double classificationMaximum() const override;
    void setClassificationMaximum( double newClassificationMaximum ) override;
    double opacity() const override;
    void setOpacity( double ) override;
    bool getDirectSourceMinMax( double &min, double &max ) const override;
    void calculateSourceMinMax( double &min, double &max ) const override;
    void onSettingsUpdated() override;

  private:
    ReosGriddedRainfallRendererFactory_p *mRendererfactory = nullptr;

};

class ReosGriddedRainfallRasterProvider_p : public QgsRasterDataProvider
{
    Q_OBJECT
    // QgsRasterInterface interface
  public:
    ReosGriddedRainfallRasterProvider_p() = default;
    ReosGriddedRainfallRasterProvider_p( const QString &uri );

    int bandCount() const override {return 1;}

    // QgsDataProvider interface
    QgsCoordinateReferenceSystem crs() const  override {return mCrs;}
    bool isValid() const {return true;}
    QString name() const {return QString();}
    QString description() const {return QString();}

    // QgsRasterDataProvider interface
    QString htmlMetadata() {return QString();}
    QString lastErrorTitle() {return QString();}
    QString lastError() {return QString();}
    Qgis::DataType sourceDataType( int bandNo ) const {return Qgis::DataType::Float64;}

    // QgsRasterInterface interface
    QgsRasterDataProvider *clone() const override;
    Qgis::DataType dataType( int bandNo ) const override {return Qgis::DataType::Float64;}
    QgsRectangle extent() const override;
    int xSize() const override;
    int ySize() const override;
    //int capabilities() const override;

    bool readBlock( int bandNo, QgsRectangle  const &viewExtent, int width, int height, void *data, QgsRasterBlockFeedback *feedback = nullptr ) override;

    void setData( const QVector<double> values );
    void setExtent( const ReosRasterExtent &extent );

  private:
    QVector<double> mValues;
    QgsCoordinateReferenceSystem mCrs;
    QgsRectangle mExtent;
    int mXCount = 0;
    int mYCount = 0;

};

class ReosGriddedRainfallProviderMetaData: public QgsProviderMetadata
{
  public:
    ReosGriddedRainfallProviderMetaData();

    ReosGriddedRainfallRasterProvider_p *createProvider(
      const QString &,
      const QgsDataProvider::ProviderOptions &,
      QgsDataProvider::ReadFlags ) override;
};


#endif // REOSGRIDDEDRAINFALLRENDERER_P_H
