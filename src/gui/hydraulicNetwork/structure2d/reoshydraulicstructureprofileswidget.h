/***************************************************************************
  reoshydraulicstructureprofileswidget.h - ReosHydraulicStructureProfilesWidget

 ---------------------
 begin                : 28.8.2022
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
#ifndef REOSHYDRAULICSTRUCTUREPROFILESWIDGET_H
#define REOSHYDRAULICSTRUCTUREPROFILESWIDGET_H

#include "reosactionwidget.h"
#include "reosguicontext.h"
#include "reosmapitem.h"

class ReosHydraulicStructure2D;
class ReosMapToolDrawPolyline;
class ReosHydraulicStructureProfile;
class ReosMapToolEditMapPolyline;
class ReosPlotCurve;
class ReosPlotPolygons;
class ReosMapToolSelectMapItem;

namespace Ui
{
  class ReosHydraulicStructureProfilesWidget;
}

class ReosHydraulicStructureProfilesWidget : public ReosStackedPageWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicStructureProfilesWidget( ReosHydraulicStructure2D *structure, const ReosGuiContext &guiContext );
    ~ReosHydraulicStructureProfilesWidget();

    virtual void showBackButton();
    virtual void hideBackButton();

    virtual bool canBeDetached() const {return false;}
    virtual void hideDetachButton();

  protected:
    void showEvent( QShowEvent *e );
    void hideEvent( QHideEvent *e );

  private slots:
    void onNewProfileAdded( const QPolygonF &profile );
    void onCurrentProfileChanged();
    void onRemoveProfile();
    void onRenameProfile();
    void onTimeChanged( const QDateTime &time );
    void onPlotCursorMove( const QPointF &pos );
    void onProfileSelected( ReosMapItem *item, const QPointF &point );
    void onCurrentProfileEdited();

  private:
    Ui::ReosHydraulicStructureProfilesWidget *ui;
    ReosGuiContext mGuiContext;
    ReosHydraulicStructure2D *mStructure = nullptr;
    ReosMapPolylineStructure mMapStructureItem;
    ReosHydraulicStructureProfile *mCurrentProfile = nullptr;

    ReosPlotCurve *mTerrainProfileCurve = nullptr;
    ReosPlotCurve *mWaterLevelProfileCurve = nullptr;
    ReosPlotCurve *mVelocityProfileCurve = nullptr;
    ReosPlotPolygons *mFilledWater = nullptr;

    QAction *mActionAddProfile = nullptr;
    ReosMapToolDrawPolyline *mMapToolAddProfile = nullptr;
    QAction *mActionSelectProfile = nullptr;
    ReosMapToolSelectMapItem *mMapToolSelectProfile = nullptr;
    QAction *mActionEditProfile = nullptr;
    ReosMapToolEditMapPolyline *mMapToolEditProfile = nullptr;
    QAction *mActionRemoveProfile = nullptr;
    QAction *mActionRenameProfile = nullptr;

    QAction *mActionDisplayVelocity = nullptr;

    using MapProfile = std::shared_ptr<ReosMapPolyline>;
    using MapProfiles = QHash<ReosHydraulicStructureProfile *, MapProfile>;

    MapProfiles mMapProfiles;

    void syncProfiles();
    void createMapProfile( int profileIndex, const QPolygonF &profile );
    void removeMapProfile( ReosHydraulicStructureProfile *profile );
    void selectProfile( ReosHydraulicStructureProfile *profile );
    void unselectProfile( ReosHydraulicStructureProfile *profile );

    void updateCurrentProfileValues();
};

#endif // REOSHYDRAULICSTRUCTUREPROFILESWIDGET_H
