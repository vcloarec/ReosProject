/***************************************************************************
  reosgribprovider.h - ReosGribProvider

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
#ifndef REOSGRIBPROVIDER_H
#define REOSGRIBPROVIDER_H

#include <QDateTime>
#include <QCache>

#include "reosgdalutils.h"
#include "reosmodule.h"
#include "reosgriddedrainfallprovider.h"
#include "reosgriddeddata.h"
#include "reosmemoryraster.h"

#define GRIB_KEY QStringLiteral("grib")


class ReosGribGriddedDataProvider : public ReosGriddedDataProvider
{
    Q_OBJECT
  public:
    ReosGribGriddedDataProvider();

    ReosGriddedDataProvider *clone() const override;
    void load() override;
    QStringList fileSuffixes() const override;
    QString key() const override {return staticKey();}
    FileDetails details( const QString &, ReosModule::Message & ) const override;
    bool isValid() const override;
    int count() const override;
    bool canReadUri( const QString &path ) const override;
    QDateTime startTime( int index ) const override;
    QDateTime endTime( int index ) const override;
    ReosRasterExtent extent() const override;
    const QVector<double> data( int index ) const override;
    bool getDirectMinMax( double &min, double &max ) const override;
    void calculateMinMax( double &min, double &max ) const override;
    QString htmlDescription() const override;
    void exportToTiff( int index, const QString &fileName ) const override;
    ReosDuration minimumTimeStep() const override;
    double timeStepRatio( int index, const ReosDuration &timeStep ) const override;

    static QString dataType();

    //! Returns the key of this provider
    static QString staticKey();

    static QString uri( const QString &sourcePath, const QString &variable, ValueType valueType );
    static QString uri( const QString &sourcePath, const QVariantMap &gribKeys, bool cumulativeOnDay = false, bool allInstantaneousFrames = false );
    static QString sourcePathFromUri( const QString &uri );
    static QString variableFromUri( const QString &uri );
    static ValueType valueTypeFromUri( const QString &uri );
    static QVariantMap keysFromUri( const QString &uri );
    static bool cumulativeOnDayFromUri( const QString &uri );
    static bool allInstanteousFrameFromUri( const QString &uri );

    bool sourceIsValid( const QString &source, ReosModule::Message &message ) const;

    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override;

  private:

    enum GribReader
    {
      GDAL,
      EcCodes
    };

    struct GribFrame
    {
      QString file;
      int frameNo = 0;
      qint64 validTime = 0;
      GribReader reader = GDAL;
      ReosDuration timeRange;
    };

    QList<GribFrame> mFrames;
    ReosRasterExtent mExtent;
    qint64 mReferenceTime = -1;
    QVariantMap mGribKeys;

    bool mIsValid = false;
    ReosModule::Message mLastMessage;

    mutable bool mHasMinMaxCalculated = false;
    mutable double mMin = std::numeric_limits<double>::max();
    mutable double mMax = -std::numeric_limits<double>::max();

    struct CacheValues
    {
      QVector<double> values;
      ValueType typeCalculatedFrom;
    };

    mutable QCache<int, CacheValues> mCache;

    void parseFileWithGDAL( const QString &fileName,
                            const QString &varName,
                            qint64 &refTime,
                            QMap<qint64, GribFrame> &pathes,
                            ReosRasterExtent &extent );

    void parseFileWithEcCodes( const QString &fileName,
                               QMap<qint64, GribFrame> &pathes,
                               ReosRasterExtent &extent );

    QStringList getFiles( const QString &path, QDir &dir ) const;

    static void giveName( FileDetails &details );

    ReosRasterMemory<double> frame( int index ) const;
    mutable std::unique_ptr<ReosGriddedDataSource> mCurrentReader;
    mutable GribReader mCurrentReaderType = GDAL;
    mutable QString mCurrentFile;
};

class ReosGribProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosGriddedDataProvider *createProvider( const QString &dataType ) const override;
    QString key() const override;
    bool supportType( const QString &dataType ) const override;
    QVariantMap uriParameters( const QString &dataType ) const override;
    QString buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const override;
};

#endif // REOSGRIBPROVIDER_H
