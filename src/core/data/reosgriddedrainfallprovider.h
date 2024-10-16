/***************************************************************************
  reosgriddedrainfallprovider.h - ReosGriddedRainfallProvider

 ---------------------
 begin                : 11.11.2022
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
#ifndef REOSGRIDDEDRAINFALLPROVIDER_H
#define REOSGRIDDEDRAINFALLPROVIDER_H

#define SIP_NO_FILE

#include "reosmodule.h"
#include "reosdataprovider.h"
#include "reosmemoryraster.h"
#include "reoscore.h"
#include "reosduration.h"

class ReosDuration;
class ReosGriddedRainfall;


class REOSCORE_EXPORT ReosGriddedRainfallProvider : public ReosDataProvider
{
    Q_OBJECT
  public:
    enum class ValueType
    {
      Intensity,
      Height,
      CumulativeHeight
    };

    enum SupportedGridOrigin
    {
      TopLeft = 1 << 0,
      BottomLeft = 1 << 1,
      TopRight = 1 << 2,
      BottomRight = 1 << 3,
      ZeroBottomLeft = 1 << 4
    };
    Q_ENUM( SupportedGridOrigin )
    Q_DECLARE_FLAGS( SupportedGridOrigins, SupportedGridOrigin )
    Q_FLAG( SupportedGridOrigins )

    enum PrecipitationGridCapability
    {
      SubGridExtract = 1 << 0,
      QualificationValue = 1 << 1,
    };
    Q_ENUM( PrecipitationGridCapability )
    Q_DECLARE_FLAGS( PrecipitationGridCapabilities, PrecipitationGridCapability )
    Q_FLAG( PrecipitationGridCapabilities )

    struct FileDetails
    {
      QStringList availableVariables;
      ReosMapExtent extent;
      QStringList files;
      QString deducedName;
    };

    ~ReosGriddedRainfallProvider();

    virtual ReosGriddedRainfallProvider *clone() const = 0;

    virtual SupportedGridOrigins supportedOrigin() const {return TopLeft;}

    QString dataSource() const;

    void setDataSource( const QString &uri );

    virtual FileDetails details( const QString &, ReosModule::Message & ) const {return FileDetails();}

    virtual bool isValid() const = 0;

    virtual int count() const = 0;

    virtual QDateTime startTime( int index ) const = 0;
    virtual QDateTime endTime( int index ) const = 0;
    ReosDuration intervalDuration( int index ) const;

    virtual const QVector<double> data( int index ) const = 0;

    virtual bool hasPrecipitationCapability( PrecipitationGridCapability capability ) const;

    virtual const QVector<double> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const {return QVector<double>();}

    virtual const QVector<double> qualifData( int index ) const;

    virtual ReosRasterExtent extent() const = 0;

    virtual void copyFrom( ReosGriddedRainfallProvider * ) {};

    virtual ReosEncodedElement encode( const ReosEncodeContext &context ) const = 0;
    virtual void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) = 0;

    virtual int dataIndex( const QDateTime &time ) const;

    virtual bool getDirectMinMax( double &, double & ) const {return false;}

    virtual void calculateMinMax( double &, double & ) const {};

    virtual bool hasData( const QString &uri, const ReosTimeWindow &timeWindow = ReosTimeWindow() ) const;

    virtual bool write(
      ReosGriddedRainfall *rainfall,
      const QString &uri,
      const ReosRasterExtent &destination,
      const ReosTimeWindow &timeWindow ) const;

  protected:
    ValueType mSourceValueType = ValueType::Height;

  private:
    QString mDataSource;
};

class REOSCORE_EXPORT ReosGriddedRainfallMemoryProvider : public ReosGriddedRainfallProvider
{
  public:
    ReosGriddedRainfallProvider *clone() const override;

    void load() override {};
    QString key() const override;
    QStringList fileSuffixes() const override {return QStringList();}
    bool isValid() const override {return true;}
    int count() const override;
    QDateTime startTime( int index ) const  override;
    QDateTime endTime( int index ) const  override;
    const QVector<double> data( int index ) const  override;
    ReosRasterExtent extent() const  override;
    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;

    void addFrame( const ReosRasterMemory<double> &raster, const QDateTime &startTime, const QDateTime &endTime );

    static QString dataType();

    //! Returns the key of this provider
    static QString staticKey();

    void setExtent( const ReosRasterExtent &newExtent );

    void copyFrom( ReosGriddedRainfallProvider *other ) override;

  private:
    struct Frame
    {
      QDateTime startTime;
      QDateTime endTime;
      ReosRasterMemory<double> raster;
    };

    QList<Frame> mRasters;
    ReosRasterExtent mExtent;
};

class ReosGriddedRainfallMemoryProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosGriddedRainfallProvider *createProvider( const QString &dataType ) const override;
    QString key() const override;
};

#endif // REOSGRIDDEDRAINFALLPROVIDER_H
