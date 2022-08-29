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

  private slots:
    void onNewProfileAdded( const QPolygonF &profile );
    void onCurrentProfileChanged();

  private:
    Ui::ReosHydraulicStructureProfilesWidget *ui;
    ReosGuiContext mGuiContext;
    ReosHydraulicStructure2D *mStructure = nullptr;
    ReosMapPolylineStructure mMapStructureItem;

    QAction *mActionAddProfile = nullptr;
    ReosMapToolDrawPolyline *mMapToolAddProfile = nullptr;
    QAction *mActionEditProfile = nullptr;
    ReosMapToolEditMapPolyline *mMapToolEditProfile = nullptr;
    QAction *mActionRemoveProfile = nullptr;
    QAction *mActionRenameProfile = nullptr;

    using MapProfile = std::shared_ptr<ReosMapPolyline>;
    using MapProfiles = QMap<ReosHydraulicStructureProfile *, MapProfile>;

    MapProfiles mMapProfiles;

    MapProfile createMapProfile( const QPolygonF &profile );
};

#endif // REOSHYDRAULICSTRUCTUREPROFILESWIDGET_H
