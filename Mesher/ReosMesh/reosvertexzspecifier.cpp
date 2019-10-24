/***************************************************************************
                      reosvertexzspecifier.cpp
                     --------------------------------------
Date                 : 01-09-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ******************************************************************************
 *                                                                            *
 *   This program is free software; you can redistribute it and/or modify     *
 *   it under the terms of the GNU General Public License as published by     *
 *   the Free Software Foundation; either version 2 of the License, or        *
 *   (at your option) any later version.                                      *
 *                                                                            *
 *****************************************************************************/

#include "reosvertexzspecifier.h"

ReosVertexZSpecifierSimple::ReosVertexZSpecifierSimple( const VertexPointer associatedVertex ):
  ReosVertexZSpecifier( associatedVertex )
{
  mZValue = 0;
  mDirty = false;
}

ReosVertexZSpecifierSimple::ReosVertexZSpecifierSimple( const VertexPointer associatedVertex, double z ): ReosVertexZSpecifier( associatedVertex )
{
  mZValue = z;
  mDirty = false;
}

ReosVertexZSpecifier *ReosVertexZSpecifierSimple::clone( VertexPointer associatedVertex ) const
{
  return new ReosVertexZSpecifierSimple( associatedVertex, mZValue );
}

ReosVertexZSpecifier::Data ReosVertexZSpecifierSimple::data() const
{
  Data returnData;
  returnData.type = "simple";
  returnData.doubleData.push_back( mZValue );

  return returnData;

}


ReosVertexZSpecifier::ReosVertexZSpecifier( const VertexPointer associatedVertex ):
  mAssociatedVertex( associatedVertex )
{

}

ReosVertexZSpecifier::~ReosVertexZSpecifier() {}


double ReosVertexZSpecifier::zValue() const
{
  std::lock_guard<std::mutex> g( mMutex );
  if ( mDirty )
    calculateZValue();
  mDirty = false;
  return mZValue;
}

void ReosVertexZSpecifier::setDirty()
{
  std::lock_guard<std::mutex> g( mMutex );
  mDirty = true;
}

void ReosVertexZSpecifier::hasToBeRemove() {}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifier::surrogateZSpecifier( VertexPointer )
{
  return std::unique_ptr<ReosVertexZSpecifier>( mAssociatedVertex->releaseZSpecifier() );
}

ReosVertexZSpecifier::Data ReosVertexZSpecifier::data() const
{
  Data d;
  d.type = "none";
  return d;
}

void ReosVertexZSpecifier::calculateZValue() const {}


ReosVertexZSpecifierOtherVertexAndSlope::ReosVertexZSpecifierOtherVertexAndSlope( VertexPointer associatedVertex, VertexPointer otherVertex, double slope ):
  ReosVertexZSpecifierDependOnOtherVertex( associatedVertex, otherVertex ), mSlope( slope )
{

}

ReosVertexZSpecifier *ReosVertexZSpecifierOtherVertexAndSlope::clone( VertexPointer associatedVertex ) const
{
  return new ReosVertexZSpecifierOtherVertexAndSlope( associatedVertex, mOtherVertex, mSlope );
}

ReosVertexZSpecifier::Data ReosVertexZSpecifierOtherVertexAndSlope::data() const
{
  Data returnData;
  returnData.type = "vertexSlope";
  returnData.doubleData.push_back( mSlope );
  returnData.otherVertices.push_back( mOtherVertex );
  return returnData;
}

void ReosVertexZSpecifierOtherVertexAndSlope::calculateZValue() const
{
  if ( mOtherVertex )
    mZValue = mOtherVertex->z() + mSlope * mOtherVertex->distanceFrom( *mAssociatedVertex );
  else
    mZValue = INVALID_VALUE;
}


ReosVertexZSpecifierOtherVertexAndGap::ReosVertexZSpecifierOtherVertexAndGap( VertexPointer associatedVertex, VertexPointer otherVertex, double gap ):
  ReosVertexZSpecifierDependOnOtherVertex( associatedVertex, otherVertex ), mGap( gap )
{

}

ReosVertexZSpecifier *ReosVertexZSpecifierOtherVertexAndGap::clone( VertexPointer associatedVertex ) const
{
  return new ReosVertexZSpecifierOtherVertexAndGap( associatedVertex, mOtherVertex, mGap );
}

ReosVertexZSpecifier::Data ReosVertexZSpecifierOtherVertexAndGap::data() const
{
  Data returnData;
  returnData.type = "vertexGap";
  returnData.doubleData.push_back( mGap );
  returnData.otherVertices.push_back( mOtherVertex );
  return returnData;

}

void ReosVertexZSpecifierOtherVertexAndGap::calculateZValue() const
{
  if ( mOtherVertex )
    mZValue = mOtherVertex->z() + mGap;
  else
    mZValue = INVALID_VALUE;
}


ReosVertexZSpecifierDependOnOtherVertex::ReosVertexZSpecifierDependOnOtherVertex( VertexPointer associatedVertex, VertexPointer otherVertex ):
  ReosVertexZSpecifier( associatedVertex ), mOtherVertex( otherVertex )
{
  if ( otherVertex && associatedVertex )
    otherVertex->addDependentVertex( associatedVertex );
}

ReosVertexZSpecifierDependOnOtherVertex::~ReosVertexZSpecifierDependOnOtherVertex()
{
}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierDependOnOtherVertex::surrogateZSpecifier( VertexPointer vertexRemoved )
{
  if ( vertexRemoved == mOtherVertex )
  {
    hasToBeRemove();
    return std::make_unique<ReosVertexZSpecifierSimple>( mAssociatedVertex, mZValue );
  }
  else
    return ReosVertexZSpecifier::surrogateZSpecifier( vertexRemoved );
}

void ReosVertexZSpecifierDependOnOtherVertex::hasToBeRemove()
{
  mOtherVertex->removeDependentVertex( mAssociatedVertex );
}

ReosVertexZSpecifierSimpleFactory::ReosVertexZSpecifierSimpleFactory( double zValue ): ReosVertexZSpecifierFactory(), mZValue( zValue )
{

}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierSimpleFactory::createZSpecifier( const VertexPointer associatedVertex ) const
{
  return std::make_unique<ReosVertexZSpecifierSimple>( associatedVertex, mZValue );
}

bool ReosVertexZSpecifierSimpleFactory::IsCompatibleZSpecifier( const VertexPointer associatedVertex ) const
{
  ( void )associatedVertex;
  return true;
}

ReosVertexZSpecifierInterpolationFactory::ReosVertexZSpecifierInterpolationFactory() {}

ReosVertexZSpecifierInterpolationFactory::ReosVertexZSpecifierInterpolationFactory( VertexPointer firstVertex, VertexPointer secondVertex ):
  ReosVertexZSpecifierFactory(), mFirstExtremity( firstVertex ), mSecondExtremity( secondVertex )
{

}

void ReosVertexZSpecifierInterpolationFactory::setExtremitiesVertices( VertexPointer firstVertex, VertexPointer secondVertex )
{
  mInterpolatedVertices.clear();
  mInsertPosition = mInterpolatedVertices.begin();

  if ( !firstVertex || !secondVertex )
  {
    mFirstExtremity = firstVertex;
    mSecondExtremity = secondVertex;
    return;
  }

  if ( firstVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::Interpolator &&
       secondVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::Interpolator )
  {
    auto firstSpec = static_cast<ReosVertexZSpecifierInterpolation *>( firstVertex->zSpecifier() );
    auto secondSpec = static_cast<ReosVertexZSpecifierInterpolation *>( secondVertex->zSpecifier() );

    if ( firstSpec->nextVertexInInterpolation() == secondSpec->previousVertexInInterpolation() ||
         secondSpec->nextVertexInInterpolation() == firstSpec->previousVertexInInterpolation() )
      //the two extremity are folowing each other in the same interpolation
      //--> the new interpolated Zspecifier will be part of the existant interpolation
      //    the new specifier will be inserted after the first vertex in the list among firstvertex and secondVertex
    {
      //search wich vertex is the first
      if ( firstSpec->previousVertexInInterpolation() == secondVertex )
      {
        firstVertex = secondVertex;
        firstSpec = secondSpec;
      }

      //reconstruct the list of all interpolated vertices and extract the extremity
      setInterpolatedVertex( firstVertex );

      return;
    }
  }

  if ( firstVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::Interpolator )
  {
    auto firstSpec = static_cast<ReosVertexZSpecifierInterpolation *>( firstVertex->zSpecifier() );

    if ( firstSpec->firstExtremity() == secondVertex || firstSpec->secondExtremity() == secondVertex )
      //the two extremity are folowing each other in the same interpolation and secondVertex is one of the extremity of the interpolation
      //--> the new interpolated Zspecifier will be part of the existant interpolation
      //    the new specifier will be inserted after or before the fisrtVertex depending on the direction of the interpolation
    {
      //reconstruct the list of all interpolated vertices and extract the extremity
      setInterpolatedVertex( firstVertex );
      if ( firstSpec->firstExtremity() == secondVertex )
        mInsertPosition--;

      return;
    }
  }

  if ( secondVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::Interpolator )
  {
    auto secondSpec = static_cast<ReosVertexZSpecifierInterpolation *>( secondVertex->zSpecifier() );

    if ( secondSpec->firstExtremity() == firstVertex || secondSpec->secondExtremity() == firstVertex )
      //the two extremity are folowing each other in the same interpolation and fisrtVertex is one of the extremity of the interpolation
      //--> the new interpolated Zspecifier will be part of the existant interpolation
      //    the new specifier will be inserted after or before the fisrtVertex depending on the direction of the interpolation
    {
      //reconstruct the list of all interpolated vertices and extract the extremity

      setInterpolatedVertex( secondVertex );
      if ( secondSpec->firstExtremity() == firstVertex )
        mInsertPosition--;

      return;
    }
  }


  mFirstExtremity = firstVertex;
  mSecondExtremity = secondVertex;

}

bool ReosVertexZSpecifierInterpolationFactory::IsCompatibleZSpecifier( const VertexPointer associatedVertex ) const
{
  if ( !mFirstExtremity || !mSecondExtremity )
    return false;

  if ( mFirstExtremity->zSpecifier()->type() == ReosVertexZSpecifier::Type::VertexAndGap ||
       mFirstExtremity->zSpecifier()->type() == ReosVertexZSpecifier::Type::VertexAndSlope )
  {
    ReosVertexZSpecifierDependOnOtherVertex *specOtherVertex = static_cast<ReosVertexZSpecifierDependOnOtherVertex *>( mFirstExtremity->zSpecifier() );
    if ( specOtherVertex->otherVertex() == associatedVertex )
      return false;
  }

  if ( mSecondExtremity->zSpecifier()->type() == ReosVertexZSpecifier::Type::VertexAndGap ||
       mSecondExtremity->zSpecifier()->type() == ReosVertexZSpecifier::Type::VertexAndSlope )
  {
    ReosVertexZSpecifierDependOnOtherVertex *specOtherVertex = static_cast<ReosVertexZSpecifierDependOnOtherVertex *>( mSecondExtremity->zSpecifier() );
    if ( specOtherVertex->otherVertex() == associatedVertex )
      return false;
  }
  return true;
}


std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierInterpolationFactory::createZSpecifier( const VertexPointer associatedVertex ) const
{
  if ( associatedVertex == mFirstExtremity || associatedVertex == mSecondExtremity )
  {
    //the associated vertex is one of the extremity, so do not create a new specifier but return the existant one
    return std::unique_ptr<ReosVertexZSpecifier>( associatedVertex->releaseZSpecifier() );
  }

  VertexPointer previous = nullptr;
  if ( mInsertPosition != mInterpolatedVertices.begin() )
  {
    auto it = mInsertPosition;
    it--;
    previous = *it;
  }

  VertexPointer next = nullptr;
  if ( mInsertPosition != mInterpolatedVertices.end() )
  {
    next = ( *mInsertPosition );
  }

  auto spec = std::make_unique<ReosVertexZSpecifierInterpolation>( associatedVertex, mFirstExtremity, mSecondExtremity, previous, next );

  mInterpolatedVertices.insert( mInsertPosition, associatedVertex );
  return spec;
}

ReosVertexZSpecifierInterpolation::ReosVertexZSpecifierInterpolation( const VertexPointer associatedVertex,
    VertexPointer firstVertex, VertexPointer secondVertex,
    VertexPointer previous, VertexPointer next, bool withAssociation ):
  ReosVertexZSpecifier( associatedVertex ),
  mFirstExtremity( firstVertex ), mSecondExtremity( secondVertex ),
  mPreviousVertexInterpolation( previous ), mNextVertexInterpolation( next )
{
  firstVertex->addDependentVertex( associatedVertex );
  secondVertex->addDependentVertex( associatedVertex );
  if ( withAssociation )
  {
    if ( previous )
    {
      auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( previous->zSpecifier() );
      spec->mNextVertexInterpolation = mAssociatedVertex;
    }

    if ( next )
    {
      auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( next->zSpecifier() );
      spec->mPreviousVertexInterpolation = mAssociatedVertex;
    }
  }

}

ReosVertexZSpecifierInterpolation::~ReosVertexZSpecifierInterpolation()
{


}

VertexPointer ReosVertexZSpecifierInterpolation::firstExtremity() const {return mFirstExtremity;}

VertexPointer ReosVertexZSpecifierInterpolation::secondExtremity() const {return mSecondExtremity;}

VertexPointer ReosVertexZSpecifierInterpolation::previousVertexInInterpolation() const
{
  return mPreviousVertexInterpolation;
}

VertexPointer ReosVertexZSpecifierInterpolation::nextVertexInInterpolation() const
{
  return mNextVertexInterpolation;
}

void ReosVertexZSpecifierInterpolation::changeFirstExtremity( VertexPointer vertexExtremity )
{
  auto currentVertex = mAssociatedVertex;
  while ( currentVertex )
  {
    auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( currentVertex->zSpecifier() );
    spec->setFirstExtremity( vertexExtremity );
    currentVertex = spec->mNextVertexInterpolation;
  }
}

void ReosVertexZSpecifierInterpolation::changeSecondExtremity( VertexPointer vertexExtremity )
{
  auto currentVertex = mAssociatedVertex;
  while ( currentVertex )
  {
    auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( currentVertex->zSpecifier() );
    spec->setSecondExtremity( vertexExtremity );
    currentVertex = spec->mPreviousVertexInterpolation;
  }
}

void ReosVertexZSpecifierInterpolation::setFirstExtremity( VertexPointer vertexExtremity )
{
  mFirstExtremity->removeDependentVertex( mAssociatedVertex );
  mFirstExtremity = vertexExtremity;
  mFirstExtremity->addDependentVertex( mAssociatedVertex );
}

void ReosVertexZSpecifierInterpolation::setSecondExtremity( VertexPointer vertexExtremity )
{
  mSecondExtremity->removeDependentVertex( mAssociatedVertex );
  mSecondExtremity = vertexExtremity;
  mSecondExtremity->addDependentVertex( mAssociatedVertex );
}

ReosVertexZSpecifier::Data ReosVertexZSpecifierInterpolation::data() const
{
  Data returnData;
  returnData.type = "interpolator";
  returnData.otherVertices.push_back( mFirstExtremity );
  returnData.otherVertices.push_back( mSecondExtremity );

  if ( mPreviousVertexInterpolation )
    returnData.otherVertices.push_back( mPreviousVertexInterpolation );
  else
    returnData.otherVertices.push_back( nullptr );

  if ( mNextVertexInterpolation )
    returnData.otherVertices.push_back( mNextVertexInterpolation );
  else
    returnData.otherVertices.push_back( nullptr );

  return returnData;
}

ReosVertexZSpecifier *ReosVertexZSpecifierInterpolation::clone( VertexPointer associatedVertex ) const
{
  return new ReosVertexZSpecifierInterpolation( associatedVertex, mFirstExtremity, mSecondExtremity, mPreviousVertexInterpolation, mNextVertexInterpolation );
}

void ReosVertexZSpecifierInterpolation::hasToBeRemove()
{
  if ( mPreviousVertexInterpolation )
  {
    mPreviousVertexInterpolation->zSpecifier()->zValue();//necessary to fill the cache with the z value;
    auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( mPreviousVertexInterpolation->zSpecifier() );
    spec->mNextVertexInterpolation = nullptr;
    spec->changeSecondExtremity( mAssociatedVertex );
  }

  if ( mNextVertexInterpolation )
  {
    mNextVertexInterpolation->zSpecifier()->zValue();//necessary to fill the cache with the z value;
    auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( mNextVertexInterpolation->zSpecifier() );
    spec->mPreviousVertexInterpolation = nullptr;
    spec->changeFirstExtremity( mAssociatedVertex );
  }

  mFirstExtremity->removeDependentVertex( mAssociatedVertex );
  mSecondExtremity->removeDependentVertex( mAssociatedVertex );
}


void ReosVertexZSpecifierInterpolation::calculateZValue() const
{
  std::list<VertexPointer> vertices = verticesList();

  double distance = 0;
  auto pv = vertices.begin();
  auto p = pv;
  p++;
  while ( p != vertices.end() )
  {
    distance += ( *pv )->distanceFrom( *( *p ) );
    pv = p;
    p++;
  }

  double firstZValue = vertices.front()->z();
  double gap = firstZValue - vertices.back()->z();
  double slope = gap / distance;


  pv = vertices.begin();
  p = pv;
  p++;
  auto lp = vertices.end();
  lp--;
  double cumul = 0;
  while ( p != lp )
  {
    auto currentSpecifier = static_cast<ReosVertexZSpecifierInterpolation *>( ( *p )->zSpecifier() );
    if ( ( *p ) != mAssociatedVertex )
      std::lock_guard<std::mutex> g( currentSpecifier->mMutex );
    currentSpecifier->mDirty = false;
    cumul += ( *pv )->distanceFrom( *( *p ) );
    currentSpecifier->mZValue = firstZValue - cumul * slope;
    pv = p;
    p++;
  }

}

std::list<VertexPointer> ReosVertexZSpecifierInterpolation::verticesList() const
{
  std::list<VertexPointer> vertices;
  vertices.push_back( mAssociatedVertex );

  //go previously to retrieve the previous vertices
  auto currentVertex = mPreviousVertexInterpolation;
  while ( currentVertex )
  {
    vertices.push_front( currentVertex );
    auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( currentVertex->zSpecifier() );
    currentVertex = spec->mPreviousVertexInterpolation;
  }

  //go next to retrieve the nect vertices
  currentVertex = mNextVertexInterpolation;
  while ( currentVertex )
  {
    vertices.push_back( currentVertex );
    auto spec = static_cast<ReosVertexZSpecifierInterpolation *>( currentVertex->zSpecifier() );
    currentVertex = spec->mNextVertexInterpolation;
  }

  vertices.push_front( mFirstExtremity );
  vertices.push_back( mSecondExtremity );

  return vertices;
}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierInterpolation::surrogateZSpecifier( VertexPointer vertexRemoved )
{
  //the removed vertex is an extremity and this interpolated vertex is the neighbor of this extremity
  if ( ( vertexRemoved == mFirstExtremity && nullptr == mPreviousVertexInterpolation )
       || ( vertexRemoved == mSecondExtremity && nullptr == mNextVertexInterpolation ) )
  {
    hasToBeRemove();
    return std::make_unique<ReosVertexZSpecifierSimple>( mAssociatedVertex, mZValue );
  }
  else
    return ReosVertexZSpecifier::surrogateZSpecifier( vertexRemoved );
}

ReosVertexZSpecifierOtherVertexAndGapFactory::ReosVertexZSpecifierOtherVertexAndGapFactory() {}

ReosVertexZSpecifierOtherVertexAndGapFactory::ReosVertexZSpecifierOtherVertexAndGapFactory( VertexPointer otherVertex, double gap ):
  ReosVertexZSpecifierDependOnOtherVertexFactory( otherVertex ), mGap( gap )
{}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierOtherVertexAndGapFactory::createZSpecifier( const VertexPointer associatedVertex ) const
{
  return std::make_unique<ReosVertexZSpecifierOtherVertexAndGap>( associatedVertex, mOtherVertex, mGap );
}

void ReosVertexZSpecifierOtherVertexAndGapFactory::setGap( double gap )
{
  mGap = gap;
}

ReosVertexZSpecifierOtherVertexAndSlopeFactory::ReosVertexZSpecifierOtherVertexAndSlopeFactory() {}

ReosVertexZSpecifierOtherVertexAndSlopeFactory::ReosVertexZSpecifierOtherVertexAndSlopeFactory( VertexPointer otherVertex, double slope ):
  ReosVertexZSpecifierDependOnOtherVertexFactory( otherVertex ), mSlope( slope )
{}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierOtherVertexAndSlopeFactory::createZSpecifier( const VertexPointer associatedVertex ) const
{
  return std::make_unique<ReosVertexZSpecifierOtherVertexAndSlope>( associatedVertex, mOtherVertex, mSlope );
}

void ReosVertexZSpecifierOtherVertexAndSlopeFactory::setSlope( double slope )
{
  mSlope = slope;
}

ReosVertexZSpecifierDependOnOtherVertexFactory::ReosVertexZSpecifierDependOnOtherVertexFactory() {}

ReosVertexZSpecifierDependOnOtherVertexFactory::ReosVertexZSpecifierDependOnOtherVertexFactory( VertexPointer otherVertex ): ReosVertexZSpecifierFactory(), mOtherVertex( otherVertex )
{

}

ReosVertexZSpecifierDependOnOtherVertexFactory::~ReosVertexZSpecifierDependOnOtherVertexFactory() {}

void ReosVertexZSpecifierDependOnOtherVertexFactory::setOtherVertex( VertexPointer otherVertex )
{
  mOtherVertex = otherVertex;
}

bool ReosVertexZSpecifierDependOnOtherVertexFactory::IsCompatibleZSpecifier( const VertexPointer associatedVertex ) const
{
  if ( !mOtherVertex )
    return false;

  if ( mOtherVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::VertexAndGap ||
       mOtherVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::VertexAndSlope )
  {
    ReosVertexZSpecifierDependOnOtherVertex *specOtherVertex = static_cast<ReosVertexZSpecifierDependOnOtherVertex *>( mOtherVertex->zSpecifier() );
    if ( specOtherVertex->otherVertex() == associatedVertex )
      return false;
  }

  if ( mOtherVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::Interpolator )
  {
    //verify if the associated vertex is not an extremity of the interpolated vertex mOtherVertex
    ReosVertexZSpecifierInterpolation *specOtherVertex = static_cast<ReosVertexZSpecifierInterpolation *>( mOtherVertex->zSpecifier() );
    if ( associatedVertex == specOtherVertex->firstExtremity() || associatedVertex == specOtherVertex->secondExtremity() )
      return false;

    //verify if the associated vertex is not actually an interpolated vertex linked to the interpolated vertex mOtherVertex
    //first check if the assocaited vertex is an interpolated vertex
    if ( associatedVertex->zSpecifier()->type() == ReosVertexZSpecifier::Type::Interpolator )
    {
      //then go previously
      VertexPointer cv = specOtherVertex->previousVertexInInterpolation();
      bool found = false;
      while ( cv && !found )
      {
        found = ( cv == associatedVertex );
        cv = static_cast<ReosVertexZSpecifierInterpolation *>( cv->zSpecifier() )->previousVertexInInterpolation();
      }
      if ( found )
        return false;
      //then go next
      cv = specOtherVertex->nextVertexInInterpolation();
      while ( cv && !found )
      {
        found = ( cv == associatedVertex );
        cv = static_cast<ReosVertexZSpecifierInterpolation *>( cv->zSpecifier() )->nextVertexInInterpolation();
      }
      if ( found )
        return false;
    }

  }


  return true;
}

ReosVertexZSpecifierFactory::~ReosVertexZSpecifierFactory() {}

void ReosVertexZSpecifierGeneralFactory::setData( const ReosVertexZSpecifier::Data &data )
{
  if ( "simple" == data.type )
  {
    if ( data.doubleData.size() > 0 )
    {
      simpleFactory.setZValue( data.doubleData.at( 0 ) );
      currentFactory = &simpleFactory;
    }
    else
      currentFactory = nullptr;

  }

  if ( "vertexSlope" == data.type )
  {
    if ( data.otherVertices.size() > 0 && data.otherVertices.at( 0 ) != nullptr && data.doubleData.size() > 0 )
    {
      slopeFactory.setOtherVertex( data.otherVertices.at( 0 ) );
      slopeFactory.setSlope( data.doubleData.at( 0 ) );
      currentFactory = &slopeFactory;
    }
    else
    {
      currentFactory = nullptr;
    }

  }

  if ( "vertexGap" == data.type )
  {
    if ( data.otherVertices.size() > 0 && data.otherVertices.at( 0 ) != nullptr && data.doubleData.size() > 0 )
    {
      gapFactory.setOtherVertex( data.otherVertices.at( 0 ) );
      gapFactory.setGap( data.doubleData.at( 0 ) );
      currentFactory = &gapFactory;
    }
    else
    {
      currentFactory = nullptr;
    }

  }

  if ( "interpolator" == data.type )
  {
    if ( data.otherVertices.size() > 3 &&
         data.otherVertices.at( 0 ) != nullptr && data.otherVertices.at( 1 ) != nullptr )
    {
      interpolationFactory.setNeighbor( data.otherVertices.at( 0 ), data.otherVertices.at( 1 ), data.otherVertices.at( 2 ), data.otherVertices.at( 3 ) );
      currentFactory = &interpolationFactory;
    }
    else
    {
      currentFactory = nullptr;
    }

  }
}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierGeneralFactory::createZSpecifier( const VertexPointer associatedVertex ) const
{
  if ( currentFactory )
    return currentFactory->createZSpecifier( associatedVertex );
  else
    return std::make_unique<ReosVertexZSpecifierSimple>( associatedVertex );

}

bool ReosVertexZSpecifierGeneralFactory::IsCompatibleZSpecifier( const VertexPointer associatedVertex ) const
{
  if ( currentFactory )
    return currentFactory->IsCompatibleZSpecifier( associatedVertex );
  else
    return false;
}

void ReosVertexZSpecifierGeneralFactory::ReosVertexZSpecifierRawInterpolatorFactory::setNeighbor( VertexPointer firstExtremityVertex, VertexPointer secondExtremityVertex, VertexPointer previousInterpolator, VertexPointer nextInterpolator )
{
  mFirstExtremity = firstExtremityVertex;
  mSecondExtremity = secondExtremityVertex;
  mPreviousInterpolator = previousInterpolator;
  mNextInterpolator = nextInterpolator;
}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierGeneralFactory::ReosVertexZSpecifierRawInterpolatorFactory::createZSpecifier( const VertexPointer associatedVertex ) const
{
  if ( associatedVertex == mFirstExtremity || associatedVertex == mSecondExtremity
       || associatedVertex == mPreviousInterpolator || associatedVertex == mNextInterpolator )
  {
    //the associated vertex is one of the extremity, so do not create a new specifier but return the existant one
    return std::unique_ptr<ReosVertexZSpecifier>( associatedVertex->releaseZSpecifier() );
  }

  return std::make_unique<ReosVertexZSpecifierInterpolation>( associatedVertex, mFirstExtremity, mSecondExtremity, mPreviousInterpolator, mNextInterpolator, false );

}
