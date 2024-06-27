/***************************************************************************
  reoscomephoresprovider.h - ReosComephoresProvider

 ---------------------
 begin                : 22.12.2022
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
#ifndef REOSCOMEPHORESPROVIDER_H
#define REOSCOMEPHORESPROVIDER_H

#include <QCache>

#include "reosgriddedrainfallprovider.h"
#include "reosnetcdfutils.h"

#define ERA5_KEY QStringLiteral("era5")

class ReosEra5FilesReader
{
  public:
    virtual  ~ReosEra5FilesReader() = default;

    virtual ReosEra5FilesReader *clone() const = 0;
    virtual int frameCount() const = 0;
    virtual QDateTime time( int i ) const = 0;
    virtual QVector<double> data( int index, bool &readLine ) const = 0;
    virtual QVector<double> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const {return QVector<double>();}
    virtual bool supportGridExtent() const {return false;}
    virtual ReosRasterExtent extent() const = 0;
    virtual bool getDirectMinMax( double &min, double &max ) const = 0;
    virtual bool hasQualif() const = 0;
    virtual QVector<int> qualifData( int index, bool &readLine ) const = 0;
};


/**
 * Class that read data in a NetCDF COMEPHORE file.
 * Specification of this format: https://www7.obs-mip.fr/wp-content-aeris/uploads/sites/59/2020/02/radar_produit_comephore.pdf
 * In the file, NetCDF dimensions are (time,X,Y). When reading data, as the last dimension is varying fastest, data are read by column.
 */
class ReosEra5NetCdfFilesReader : public ReosEra5FilesReader
{
  public:
    explicit ReosEra5NetCdfFilesReader( const QString &fileName, const QString &varName );

    ReosEra5NetCdfFilesReader *clone() const;
    int frameCount() const ;
    QDateTime time( int i ) const;
    QVector<double> data( int index, bool &readLine ) const;
    QVector<double> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const;
    bool supportGridExtent() const  {return true;}
    ReosRasterExtent extent() const ;
    bool getDirectMinMax( double &min, double &max ) const;
    bool hasQualif() const  {return true;}
    QVector<int> qualifData( int index, bool &readLine ) const ;

    static bool canReadFile( const QString &uri );

    void reset();

  private:
    ReosEra5NetCdfFilesReader() = default;
    mutable std::unique_ptr<ReosNetCdfFile> mFile;
    QString mFileName;
    QString mVarName;
    ReosRasterExtent mExtent;
    QList<QDateTime> mTimes;
    QMap<int, int> mDataIndexToFileIndex;
    double mScalefactor = std::numeric_limits<double>::quiet_NaN();
    double mAddOffset = std::numeric_limits<double>::quiet_NaN();
    qint16 mFillingValue = 0;
    qint16 mMissingValue = 0;

    QVector<double> treatRawData( const QVector<qint16> &rawData ) const;
};



class ReosEra5NetCdfFolderReader: public ReosEra5FilesReader
{
  public:
    explicit ReosEra5NetCdfFolderReader( const QString &folderPath, const QString &varName );

    ReosEra5FilesReader *clone() const override;
    int frameCount() const override;
    QDateTime time( int i ) const override;
    QVector<double> data( int index, bool &readLine ) const override;
    QVector<double> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax, bool &readLine ) const override;
    bool supportGridExtent() const override {return true;}
    ReosRasterExtent extent() const override;
    bool getDirectMinMax( double &min, double &max ) const override;
    bool hasQualif() const override {return true;}
    QVector<int> qualifData( int index, bool &readLine ) const override;

    static bool canReadFile( const QString &uri );

  private:
    QString mFolderPath;
    QString mVarName;
    std::vector<std::unique_ptr<ReosEra5NetCdfFilesReader>> mFileReaders;
    mutable size_t mLastFileIndex = -1;
    ReosRasterExtent mExtent;
    struct InternalIndex
    {
      size_t fileIndex = -1;
      int internIndex = -1;
    };
    QMap<int, InternalIndex> mGlobalIndexToReaderIndex;

    ReosEra5NetCdfFilesReader *fileReader( int index, int &interINdex ) const;
};

class ReosEra5Provider : public ReosGriddedDataProvider
{
  public:
    ReosEra5Provider();
    ~ReosEra5Provider();
    ReosGriddedDataProvider *clone() const override;
    void load() override;

    QString key() const override {return staticKey();}
    QStringList fileSuffixes() const override;
    QString htmlDescription() const override;


    bool isValid() const override;
    int count() const override;
    QDateTime startTime( int index ) const override;
    QDateTime endTime( int index ) const override;
    const QVector<double> data( int index ) const override;
    const QVector<double> dataInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const override;

    ReosRasterExtent extent() const override;
    bool canReadUri( const QString &uri ) const override;
    FileDetails details( const QString &source, ReosModule::Message &message ) const override;

    bool getDirectMinMax( double &min, double &max ) const override;
    void calculateMinMax( double &min, double &max ) const override;

    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context ) override {};
    ReosEncodedElement encode( const ReosEncodeContext &context ) const {return ReosEncodedElement();}

    static QString staticKey();
    static QString dataType();

    static  QVariantMap decodeUri( const QString &uri, bool &ok ) ;
    static  QString buildUri( const QVariantMap &parameters, bool &ok );
    static  QString buildUri( const QString &filePath, const QString &varName );
    static QString pathFromUri( const QString &uri, bool &ok );
    static QString varNameFromUri( const QString &uri, bool &ok );

  private:
    bool mIsValid = false;
    std::unique_ptr<ReosEra5FilesReader> mFileReader;
    ReosRasterExtent mExtent;
    mutable QCache<int, QVector<double>> mCache;
    GridCapabilities mCapabilities = {SubGridExtract | QualificationValue};

};

class ReosEra5ProviderFactory: public ReosDataProviderFactory
{
  public:
    ReosGriddedDataProvider *createProvider( const QString &dataType ) const override;
    QString key() const override;
    bool supportType( const QString &dataType ) const override;
    QVariantMap uriParameters( const QString &dataType ) const override;
    QString buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const override;
};

#endif // REOSCOMEPHORESPROVIDER_H
