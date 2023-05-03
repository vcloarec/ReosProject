/***************************************************************************
  reoshydraulicnetwork.h - ReosHydraulicNetwork

 ---------------------
 begin                : 20.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDRAULICNETWORK_H
#define REOSHYDRAULICNETWORK_H


#include<memory>
#include <QPointer>
#include <QHash>

#include "reoscore.h"
#include "reosmodule.h"
#include "reosdataobject.h"
#include "reosparameter.h"

class ReosHydraulicNode;
class ReosHydraulicLink;
class ReosHydraulicNetwork;
class ReosCalculationContext;
class ReosHydraulicNetworkContext;
class ReosWatershedModule;
class ReosGisEngine;
class ReosHydraulicSchemeCollection;
class ReosHydraulicScheme;
class ReosMapExtent;
class ReosMeteorologicModel;

#ifndef SIP_RUN

struct REOSCORE_EXPORT ReosHydraulicNetworkElementCompatibilty
{
  bool isCompatible = true;
  QStringList incompatibilityReasons;

  void combine( const ReosHydraulicNetworkElementCompatibilty &other )
  {
    incompatibilityReasons.append( other.incompatibilityReasons );
    isCompatible &= other.isCompatible;
  }
};

#endif //No SIP_RUN

class REOSCORE_EXPORT ReosHydraulicNetworkElement : public ReosDataObject SIP_ABSTRACT
{
    Q_OBJECT

#ifdef SIP_RUN
    SIP_CONVERT_TO_SUBCLASS_CODE
    ReosHydraulicNetworkElement *element = qobject_cast<ReosHydraulicNetworkElement *>( sipCpp );

    sipType = 0;

    if ( element )
    {
      if ( element->type() == ReosHydraulicNetworkElement::hydraulicStructure2DType() )
      {
        sipType = sipType_ReosHydraulicStructure2D;
      }

      if ( element->type() == ReosHydraulicNetworkElement::hydrographJunction() )
      {
        sipType = sipType_ReosHydrographJunction;
      }
    }
    SIP_END
#endif

  public:

    static QString hydraulicStructure2DType();
    static QString hydrographJunction();

    ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent = nullptr );
    ReosHydraulicNetworkElement( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr ) SIP_SKIP;
    virtual ~ReosHydraulicNetworkElement();

    QString type() const override {return staticType();}
    static QString staticType() {return QStringLiteral( "hydraulicNetwork" );}

    //! Returns the element name parameter
    ReosParameterString *elementNameParameter() const SIP_SKIP;

    //! Returns the element nale
    QString elementName() const;

    //! Returns the defautl base name of the element
    virtual QString defaultDisplayName() const {return type();}

    //! Destroy the element (the instance will be deleted later).
    virtual void destroy();

    //! Called when the position of the item is changed
    void positionChanged();

    //! Returns the parameter of duration used as time step in table when constant time step is used
    ReosParameterDuration *constantTimeStepInTable() const SIP_SKIP;

    //! Returns the parameter whether constant tule step is used in table
    ReosParameterBoolean *useConstantTimeStepInTable() const SIP_SKIP;

    //! Returns whether calculation is in progress, default implementation returns false
    virtual bool calculationInProgress() const {return false;}

    //! Returns the maximum progression of calculation
    virtual int calculationMaxProgression() const {return 0;}

    //! Returns the progression of calculation
    virtual int calculationProgression() const {return 0;}

    ReosEncodedElement encode( const ReosHydraulicNetworkContext &context ) const SIP_SKIP;

    //! Notifies a message to the element
    void notify( const ReosModule::Message &message );

    //! Returns the last message
    ReosModule::Message lastMessage() const;

    //! Returns whether the element is auto selectable on a map
    virtual bool isAutoSelectable() const {return true;}

    //! Returns whether the element us removable
    virtual bool isRemovable() const {return true;}

    virtual void saveConfiguration( ReosHydraulicScheme *scheme ) const SIP_SKIP;
    virtual void restoreConfiguration( ReosHydraulicScheme *scheme ) SIP_SKIP;

//! Cleans all data related to \a scheme, returns a list of files or folder that should be removec when saving the project
    virtual QFileInfoList cleanScheme( ReosHydraulicScheme *scheme ) SIP_SKIP;

    //! Returns the current element time step
    virtual ReosDuration currentElementTimeStep() const;

    //! Returns the element time window
    virtual ReosTimeWindow timeWindow() const;

    //! Returns the element time step related to map rendering
    virtual ReosDuration mapTimeStep() const;

    //! Return a pointer to the networl of the element
    ReosHydraulicNetwork *network() const;

    //! Returns the extent of the element
    virtual ReosMapExtent extent() const = 0;

    //! Returns a icon representing the element
    virtual QIcon icon() const {return QIcon();}

    //! Returns information about the compatibilty of this element with the \a scheme
    virtual ReosHydraulicNetworkElementCompatibilty checkCompatiblity( ReosHydraulicScheme *scheme ) const SIP_SKIP;

  public slots:
    //! Update the calculation context
    virtual void updateCalculationContext( const ReosCalculationContext &context ) = 0 SIP_SKIP;

  signals:
    //! Emitted whrn the time window of the element is changed
    void timeWindowChanged();

    //! Emitted whrn the map time step of the element is changed
    void mapTimeStepChanged();

  protected:

#ifndef SIP_RUN
    QPointer<ReosHydraulicNetwork> mNetwork = nullptr;
    virtual void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext &context ) const = 0;
    void calculationUpdated();

#endif //No SIP_RUN

  signals:
    void calculationStart();
    void calculationIsUpdated( const QString &id, QPrivateSignal ) SIP_SKIP;
    void timeStepChanged();
    void dirtied();

  private:
    ReosParameterString *mNameParameter = nullptr;
    ReosParameterDuration *mConstantTimeStepInTable = nullptr;
    ReosParameterBoolean *mUseConstantTimeStepInTable = nullptr;
    ReosModule::Message mLastMessage;

    void init();

};

class REOSCORE_EXPORT ReosHydraulicNetworkContext
{
  public:
    ReosHydraulicNetworkContext() = default;
    ReosWatershedModule *watershedModule() const SIP_SKIP;
    ReosHydraulicNetwork *network() const;

    QString crs() const;

    QString projectPath() const;
    QString projectName() const;

    ReosEncodeContext encodeContext() const SIP_SKIP;

    QString currentSchemeId() const;

  private:

    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
    QString mProjectPath;
    QString mProjectName;

    friend class ReosHydraulicNetwork;

};

#ifndef SIP_RUN
class ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicNetworkElementFactory();
    virtual ~ReosHydraulicNetworkElementFactory();

    //! Decodes hydraulic element from \a encodedElement and and it to the network of \a context
    virtual ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const = 0;
};
#endif //No SIP_RUN

class REOSCORE_EXPORT ReosHydraulicNetwork : public ReosModule
{
    Q_OBJECT
  public:
    ReosHydraulicNetwork() = default;
    ReosHydraulicNetwork( ReosModule *parent, ReosGisEngine *gisEngine, ReosWatershedModule *watershedModule );

    QFileInfoList uselessFiles( bool clean ) const override;

    //! Returns element with id \a elementId
    ReosHydraulicNetworkElement *getElement( const QString &elemId ) const;

    //! Returns the count of elements present in the network
    int elementsCount() const;

    //! Returns a list of elements of \a type. Returns all element if type is empty or not prvided
    QList<ReosHydraulicNetworkElement *> hydraulicNetworkElements( const QString &type = QString() ) const;

    ReosHydraulicNetworkElement *addElement( ReosHydraulicNetworkElement *elem, bool select = true ) SIP_SKIP;;

    //! Removes the element \a elem. The element is destoyed by calling this method.
    void removeElement( ReosHydraulicNetworkElement *elem );

    void decode( const ReosEncodedElement &element, const QString &projectPath, const QString &projectFileName ) SIP_SKIP;
    ReosEncodedElement encode( const QString &projectPath, const QString &projectFileName ) const SIP_SKIP;

    //! Clears the network
    void clear();

    //! Reset the network, that is, clear all and creates just one hydraulic scheme
    void reset();

    ReosGisEngine *gisEngine() const;

    //! Returns an hydraulic network context corresponding to this network
    ReosHydraulicNetworkContext context() const;

    ReosCalculationContext calculationContext() const;

    ReosHydraulicSchemeCollection *hydraulicSchemeCollection() const SIP_SKIP;

    //! Returns the count of hydraulic scheme
    int schemeCount() const;

    //! Returns the current scheme index
    int currentSchemeIndex() const;

    //! Return the id of the scheme with \a index
    int schemeIndex( const QString &schemeId ) const;

    //! Sets the current scheme with \a schemeIndex
    void setCurrentScheme( int schemeIndex );

    //! Sets the current scheme with \a schemeId
    void setCurrentScheme( const QString &schemeId );

    //! Returns the current scheme Id
    QString currentSchemeId() const;

    //! Returns the current scheme Name
    QString currentSchemeName() const;

#ifndef SIP_RUN
    ReosHydraulicScheme *currentScheme() const;
    ReosHydraulicScheme *scheme( const QString &schemeId ) const;
    ReosHydraulicScheme *scheme( int index ) const;
    ReosHydraulicScheme *schemeByName( const QString &schemeName ) const;
    ReosHydraulicScheme *addNewScheme( const QString &schemeName, ReosMeteorologicModel *meteoModel = nullptr );
    void addExistingScheme( ReosHydraulicScheme *scheme );
    void removeScheme( int schemeIndex );
#endif

    //! Returns the current minimum time step used in the network
    ReosDuration currentTimeStep() const;

    ReosMapExtent networkExtent() const;

    //! Return time window including all element that can be rendered on map
    ReosTimeWindow mapTimeWindow() const;

    ReosHydraulicNetworkElementCompatibilty checkSchemeCompatibility( ReosHydraulicScheme *scheme ) const SIP_SKIP;

    static QString staticName() {return QStringLiteral( "hydraulic-network" );}

  signals:
    void elementAdded( ReosHydraulicNetworkElement *elem, bool select );
    void elementWillBeRemoved( ReosHydraulicNetworkElement *elem );
    void elementRemoved();
    void elementPositionHasChanged( ReosHydraulicNetworkElement *elem );
    void hasBeenReset();
    void schemeChanged();
    void loaded();
    void timeStepChanged();

  public slots:
    void changeScheme( int newSchemeIndex );

  private slots:
    void onMapTimeWindowChanged();

  private:
    ReosGisEngine *mGisEngine = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
    ReosHydraulicSchemeCollection *mHydraulicSchemeCollection = nullptr;
    int mCurrentSchemeIndex = -1;
    QHash<QString, ReosHydraulicNetworkElement *> mElements;
    mutable QString mProjectPath;
    mutable QString mProjectName;
    mutable QFileInfoList mUselessFile;

    void elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem );

    QHash<QString, int> mElementIndexesCounter;

    friend class ReosHydraulicNetworkElement;

    std::map<QString, std::unique_ptr<ReosHydraulicNetworkElementFactory>> mElementFactories;
    void addEncodedElement( const ReosEncodedElement &element );

};

#endif // REOSHYDRAULICNETWORK_H
