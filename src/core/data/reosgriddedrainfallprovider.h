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
    };

    ~ReosGriddedRainfallProvider();

    QString dataSource() const;

    virtual void setDataSource( const QString &uri );

    virtual Details details( const QString &, ReosModule::Message & ) const {return Details();}

    virtual bool isValid() const = 0;

    virtual int count() const = 0;

    virtual QDateTime startTime( int index ) const = 0;
    virtual QDateTime endTime( int index ) const = 0;
    ReosDuration intervalDuration( int index ) const;

    void setSourceValueType( ValueType valueType );

    virtual const QVector<double> data( int index ) const = 0;

    virtual ReosRasterExtent extent() const = 0;

    virtual bool canReadUri( const QString & ) const {return false;}

    virtual void copyFrom( ReosGriddedRainfallProvider * ) {};

  protected:
    ValueType mSourceValueType = ValueType::Height;

  private:
    QString mDataSource;
};

class ReosGriddedRainfallMemoryProvider : public ReosGriddedRainfallProvider
{
  public:
    QString key() const;

    QStringList availableVariables( const QString &, ReosModule::Message & ) const {return QStringList();}
    bool isValid() const {return true;}
    int count() const;
    QDateTime startTime( int index ) const;
    QDateTime endTime( int index ) const;
    const QVector<double> data( int index ) const;
    ReosRasterExtent extent() const;

    void addFrame( const ReosRasterMemory<double> &raster, const QDateTime &startTime, const QDateTime &endTime );

    static QString dataType();

    //! Returns the key of this provider
    static QString staticKey();

    void setExtent( const ReosRasterExtent &newExtent );

    void copyFrom( ReosGriddedRainfallProvider *other );

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
