/***************************************************************************
                      reoswatershedstore.cpp
                     --------------------------------------
Date                 : 04-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <QStack>
#include "reoswatershedstore.h"
#include "reoswatershed.h"

ReosWatershedStore::ReosWatershedStore()
{

}

ReosWatershed *ReosWatershedStore::addWatershed( ReosWatershed *watershed )
{
  //! Chexk if another watershed is upstream
  size_t i = 0;
  while ( i < mWatersheds.size() )
  {
    std::unique_ptr<ReosWatershed> &existingWatershed = mWatersheds.at( i );
    if ( watershed->contains( existingWatershed->outletPoint() ) )
    {
      watershed->addUpstreamWatershed( mWatersheds.at( i ).release() );
      mWatersheds.erase( mWatersheds.begin() + i );
    }
    else
      ++i;
  }

  mWatersheds.emplace_back( watershed );
  return mWatersheds.back().get();
}

ReosWatershed *ReosWatershedStore::downstreamWatershed( const QPolygonF &line, bool &ok ) const
{
  for ( const std::unique_ptr<ReosWatershed> &watershed : mWatersheds )
  {
    switch ( watershed->contains( line ) )
    {
      case ReosInclusionType::None:
        continue;
        break;
      case ReosInclusionType::Partial:
        ok = false;
        return nullptr;
        break;
      case ReosInclusionType::Total:
        ok = true;
        return watershed.get();
        break;
    }
  }

  ok = true;
  return nullptr;
}

int ReosWatershedStore::watershedCount() const
{
  int count = 0;
  for ( const std::unique_ptr<ReosWatershed> &watershed : mWatersheds )
    count += watershed->upstreamWatershedCount() + 1;

  return count;
}

int ReosWatershedStore::masterWatershedCount() const
{
  return int( mWatersheds.size() );
}
