/***************************************************************************
  reosgriddedrainitem.h - ReosGriddedRainItem

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
#ifndef REOSGRIDDEDRAINITEM_H
#define REOSGRIDDEDRAINITEM_H

#include "reosgriddedrainfallprovider.h"
#include "reosmemoryraster.h"
#include "reosrainfallitem.h"
#include "reosrenderedobject.h"
#include "reosgriddeddata.h"
#include "reos_sip.h"


class ReosRasterExtent;
class ReosGriddedRainfallProvider;
class ReosGriddedDataProvider;
class ReosGriddedRainfallRendererFactory;
class ReosColorShaderSettings;

class REOSCORE_EXPORT ReosGriddedRainfall : public ReosGriddedData
{
    Q_OBJECT
  public:
    ReosGriddedRainfall( QObject *parent = nullptr );
    ReosGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent = nullptr );
    ~ReosGriddedRainfall();

    static ReosGriddedRainfall *loadGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent = nullptr );

    //! Returns a pointer to the data provider
    ReosGriddedRainfallProvider *dataProvider() const override SIP_SKIP;

    static QString staticType();


    /**
     * Returns all the values related to \a index, order of values can be deduced from the sign of sizes dx,dy)
     *  of the cell contained in the raster extent (see extent()
     */
    const QVector<double> intensityValues( int index ) const;

    /**
     * Returns, if supported, a part of the values related to \a index, order of values can be deduced from the sign of sizes dx,dy)
     * of the cell contained in the raster extent (see extent()
     * The Returned values are from a subgrid defined by minimum anx maximum row and coulumn index.
     * width = colMax - colMin + 1
     * height = rowMax - rowMin + 1
     *
     * \see supportExtractSubGrid()
     */
    const QVector<double> intensityValuesInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const;

    //! Returns, if exist, the qualification values for \a index
    const QVector<double> qualificationData( int index ) const;

    //! Returns the part of values that are not null for the \a index. Returns value are between 0.0 and 1.0.
    double nullCoverage( int index ) const;

    //! Returns the part of qualification values that are not null for the \a index. Returns value are between 0.0 and 1.0.
    double qualifCoverage( int index, double qualif ) const;

    //! Returns a raster stored in memory containing the intensity values of the rain fall( unit: mm / h ) for the index \a index
    ReosRasterMemory<double> intensityRaster( int index ) const SIP_SKIP;

    //! Returns a grid block containing all the data for the \a index
    ReosFloat64GridBlock intensityGridBlock( int index ) const;

    //! Returns a grid block containing all the qualification data for the \a index
    ReosFloat64GridBlock qualificationGridBloc( int index ) const;

    //! Overrides the coordinates system with the wkt string \a crs of the gridded rainfall without modifying the coordinates
    void overrideCrs( const QString &crs ) SIP_SKIP;

    /**
     * Transform the gridded rain to fit with extent \a destination with resolution \a resolX and \a resolY,
     * for each time steps intersecting \a timeWindow (all time step if timeWindow is invalid).
     */
    ReosGriddedRainfall *transform( const ReosMapExtent &destination, double resolX, double resolY, const ReosTimeWindow &timeWindow = ReosTimeWindow(), QObject *parent = nullptr ) const SIP_FACTORY;

    //! Returns the current color ramp settings, keep ownership
    ReosColorShaderSettings *colorSetting() const SIP_SKIP;

    //! Sets the color ramp settings \a colorRampShader, take ownership. it the color settings is not compatible, it is destructed.
    void setColorSetting( ReosColorShaderSettings *colorRampShader ) SIP_SKIP;

    ReosEncodedElement encode( const ReosEncodeContext &context ) const SIP_SKIP;

    //! Creates new instance from the encoded element
    static ReosGriddedRainfall *decode( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent = nullptr ) SIP_SKIP;

  private:
    ReosGriddedRainfall( const ReosEncodedElement &element, const ReosEncodeContext &context, QObject *parent );

    QString formatKey( const QString &rawKey ) const;
};

#ifndef SIP_RUN

class REOSCORE_EXPORT ReosGriddedRainItem : public ReosRainfallDataItem
{
    Q_OBJECT
  public:
    //! Constructor of a gridded rain item with \a name, \a description and \a data that is a ReosGriddedRainfall, takes ownership of the rainfall
    ReosGriddedRainItem( const QString &name, const QString &description,  ReosGriddedRainfall *data );

    explicit ReosGriddedRainItem( const ReosEncodedElement &element, const ReosEncodeContext &context );

    QString dataType() const override {return ReosGriddedRainfall::staticType();}
    ReosGriddedRainfall *data() const override;
    QString information() const override {return QObject::tr( "Gridded precipitation" );}

    QIcon icone() const override;
    virtual bool accept( ReosRainfallItem *, bool = false ) const override;
    bool canBeSubItem( const ReosRainfallItem *item, bool acceptSameName ) const override;

    ReosEncodedElement encode( const ReosEncodeContext &context ) const override;

    bool isValid() const;

  private:
    ReosGriddedRainfall *mGriddedRainfall = nullptr;

};

class ReosGriddedRainfallRendererFactory
{
  public:
    ReosGriddedRainfallRendererFactory( ReosGriddedData *rainfall )
      : mGriddedData( rainfall )
    {}

    virtual ~ReosGriddedRainfallRendererFactory() = default;
    virtual ReosObjectRenderer *createRasterRenderer( ReosRendererSettings *settings ) = 0;
    virtual ReosColorShaderSettings *colorRampShaderSettings() const = 0;
    virtual void setColorRampShaderSettings( ReosColorShaderSettings *colorSettings ) = 0;
    virtual ReosEncodedElement encode() const = 0;

    ReosGriddedData *griddedData() const;

  protected:
    QPointer<ReosGriddedData> mGriddedData;
};
#endif //SIP_RUN

#endif // REOSGRIDDEDRAINITEM_H
