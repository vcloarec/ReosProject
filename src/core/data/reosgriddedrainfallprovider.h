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

#include "reosmodule.h"
#include "reosdataprovider.h"
#include "reosmemoryraster.h"
#include "reoscore.h"

class ReosDuration;

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

    struct Details
    {
      QStringList availableVariables;
      ReosMapExtent extent;
      QStringList files;
      QString deducedName;
    };

    ~ReosGriddedRainfallProvider();

    virtual ReosGriddedRainfallProvider *clone() const = 0;

    QString dataSource() const;

    virtual void setDataSource( const QString &uri );

    virtual Details details( const QString &, ReosModule::Message & ) const {return Details();}

    virtual bool isValid() const = 0;

    virtual int count() const = 0;

    virtual QDateTime startTime( int index ) const = 0;
    virtual QDateTime endTime( int index ) const = 0;
    ReosDuration intervalDuration( int index ) const;

    virtual const QVector<double> data( int index ) const = 0;

    virtual ReosRasterExtent extent() const = 0;

    virtual void copyFrom( ReosGriddedRainfallProvider * ) {};

    virtual ReosEncodedElement encode( const ReosEncodeContext &context ) const = 0;
    virtual void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) = 0;

    virtual int dataIndex( const QDateTime &time ) const;

    virtual bool getDirectMinMax( double &min, double &max ) const {return false;}

    virtual void calculateMinMax( double &min, double &max ) const {};

  protected:
    ValueType mSourceValueType = ValueType::Height;

  private:
    QString mDataSource;
};

class ReosGriddedRainfallMemoryProvider : public ReosGriddedRainfallProvider
{
  public:
    ReosGriddedRainfallProvider *clone() const override;

    QString key() const override;

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
