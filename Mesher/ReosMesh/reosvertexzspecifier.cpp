#include "reosvertexzspecifier.h"

ReosVertexZSpecifierSimple::ReosVertexZSpecifierSimple(const VertexPointer associatedVertex):
    ReosVertexZSpecifier(associatedVertex)
{
    mZValue=0;
    mDirty=false;
}

ReosVertexZSpecifierSimple::ReosVertexZSpecifierSimple(const VertexPointer associatedVertex,double z):ReosVertexZSpecifier(associatedVertex)
{
    mZValue=z;
    mDirty=false;
}

ReosVertexZSpecifier *ReosVertexZSpecifierSimple::clone(VertexPointer associatedVertex) const
{
    return new ReosVertexZSpecifierSimple(associatedVertex,mZValue);
}


ReosVertexZSpecifier::ReosVertexZSpecifier(const VertexPointer associatedVertex):
    mAssociatedVertex(associatedVertex)
{

}

ReosVertexZSpecifier::~ReosVertexZSpecifier(){}

ReosVertexZSpecifier *ReosVertexZSpecifier::clone(VertexPointer associatedVertex) const
{
    return new ReosVertexZSpecifier(associatedVertex);
}

double ReosVertexZSpecifier::zValue() const
{
    std::lock_guard<std::mutex> g(mMutex);
    if (mDirty)
        calculateZValue();
    mDirty=false;
    return mZValue;
}

void ReosVertexZSpecifier::setDirty()
{
    std::lock_guard<std::mutex> g(mMutex);
    mDirty=true;
}


ReosVertexZSpecifierOtherVertexAndSlope::ReosVertexZSpecifierOtherVertexAndSlope(VertexPointer associatedVertex, VertexPointer otherVertex, double slope):
    ReosVertexZSpecifierDependOnOtherVertex(associatedVertex,otherVertex),mSlope(slope)
{

}

ReosVertexZSpecifier *ReosVertexZSpecifierOtherVertexAndSlope::clone(VertexPointer associatedVertex) const
{
    return new ReosVertexZSpecifierOtherVertexAndSlope(associatedVertex,mOtherVertex,mSlope);
}

void ReosVertexZSpecifierOtherVertexAndSlope::calculateZValue() const
{
    if(mOtherVertex)
        mZValue=mOtherVertex->z()+mSlope*mOtherVertex->distanceFrom(*mAssociatedVertex);
    else
        mZValue=INVALID_VALUE;
}


ReosVertexZSpecifierOtherVertexAndGap::ReosVertexZSpecifierOtherVertexAndGap(VertexPointer associatedVertex, VertexPointer otherVertex, double gap):
    ReosVertexZSpecifierDependOnOtherVertex(associatedVertex,otherVertex),mGap(gap)
{

}

ReosVertexZSpecifier *ReosVertexZSpecifierOtherVertexAndGap::clone(VertexPointer associatedVertex) const
{
    return new ReosVertexZSpecifierOtherVertexAndGap(associatedVertex,mOtherVertex,mGap);
}


ReosVertexZSpecifierDependOnOtherVertex::ReosVertexZSpecifierDependOnOtherVertex(VertexPointer associatedVertex, VertexPointer otherVertex):
    ReosVertexZSpecifier (associatedVertex),mOtherVertex(otherVertex)
{
    if (otherVertex && associatedVertex)
        otherVertex->addDependentVertex(associatedVertex);
}

ReosVertexZSpecifierSimpleFactory::ReosVertexZSpecifierSimpleFactory(double zValue):ReosVertexZSpecifierFactory(),mZValue(zValue)
{

}

std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierSimpleFactory::createZSpecifier(const VertexPointer associatedVertex) const
{
    return std::make_unique<ReosVertexZSpecifierSimple>(associatedVertex,mZValue);
}

ReosVertexZSpecifierInterpolationFactory::ReosVertexZSpecifierInterpolationFactory() {}

ReosVertexZSpecifierInterpolationFactory::ReosVertexZSpecifierInterpolationFactory(VertexPointer firstVertex, VertexPointer secondVertex):
    ReosVertexZSpecifierFactory(),mFirstExtremity(firstVertex),mSecondExtremity(secondVertex)
{

}

void ReosVertexZSpecifierInterpolationFactory::setExtremitiesVertices(VertexPointer firstVertex, VertexPointer secondVertex)
{
    mFirstExtremity=firstVertex;
    mSecondExtremity=secondVertex;
}


std::unique_ptr<ReosVertexZSpecifier> ReosVertexZSpecifierInterpolationFactory::createZSpecifier(const VertexPointer associatedVertex) const
{
    if (associatedVertex==mFirstExtremity || associatedVertex==mSecondExtremity)
    {
        //the associated vertex is one of the extremity, so do not create a new specifier but return the existant one
        return std::unique_ptr<ReosVertexZSpecifier>(associatedVertex->releaseZSpecifier());
    }

    ReosVertexZSpecifierInterpolation* previous=nullptr;
    if ( ! mAddedSpecifier.empty())
        previous=mAddedSpecifier.back();

    ReosVertexZSpecifierInterpolation* next=nullptr;

    auto spec=std::make_unique<ReosVertexZSpecifierInterpolation>(associatedVertex,mFirstExtremity,mSecondExtremity,previous, next);

    mAddedSpecifier.push_back(spec.get());
    return spec;
}

ReosVertexZSpecifierInterpolation::ReosVertexZSpecifierInterpolation(const VertexPointer associatedVertex,
                                                             VertexPointer firstVertex, VertexPointer secondVertex,
                                                             ReosVertexZSpecifierInterpolation *previous, ReosVertexZSpecifierInterpolation *next):
    ReosVertexZSpecifier(associatedVertex),
    mFirstExtremity(firstVertex),mSecondExtremity(secondVertex),
    mPreviousSpecifierInterpolation(previous),mNextSpecifierInterpolation(next)
{
    firstVertex->addDependentVertex(associatedVertex);
    secondVertex->addDependentVertex(associatedVertex);
    if(previous)
        previous->mNextSpecifierInterpolation=this;
    if (next)
        next->mPreviousSpecifierInterpolation=this;
}

ReosVertexZSpecifierInterpolation::~ReosVertexZSpecifierInterpolation()
{


}

void ReosVertexZSpecifierInterpolation::changeFirstExtremity(VertexPointer vertexExtremity)
{
    auto currentSpecifier=this;
    while(currentSpecifier)
    {
        currentSpecifier->setFirstExtremity(vertexExtremity);
        currentSpecifier=currentSpecifier->mNextSpecifierInterpolation;
    }
}

void ReosVertexZSpecifierInterpolation::changeSecondExtremity(VertexPointer vertexExtremity)
{
    auto currentSpecifier=this;
    while(currentSpecifier)
    {
        currentSpecifier->setSecondExtremity(vertexExtremity);
        currentSpecifier=currentSpecifier->mPreviousSpecifierInterpolation;
    }
}

void ReosVertexZSpecifierInterpolation::setFirstExtremity(VertexPointer vertexExtremity)
{
    mFirstExtremity->removeDependentVertex(mAssociatedVertex);
    mFirstExtremity=vertexExtremity;
    mFirstExtremity->addDependentVertex(mAssociatedVertex);
}

void ReosVertexZSpecifierInterpolation::setSecondExtremity(VertexPointer vertexExtremity)
{
    mSecondExtremity->removeDependentVertex(mAssociatedVertex);
    mSecondExtremity=vertexExtremity;
    mSecondExtremity->addDependentVertex(mAssociatedVertex);
}

ReosVertexZSpecifier *ReosVertexZSpecifierInterpolation::clone(VertexPointer associatedVertex) const
{
    return new ReosVertexZSpecifierInterpolation(associatedVertex,mFirstExtremity,mSecondExtremity,mPreviousSpecifierInterpolation,mNextSpecifierInterpolation);
}

void ReosVertexZSpecifierInterpolation::hasToBeRemove()
{
    if (mPreviousSpecifierInterpolation)
    {
        mPreviousSpecifierInterpolation->mNextSpecifierInterpolation=nullptr;
        mPreviousSpecifierInterpolation->changeSecondExtremity(mAssociatedVertex);
    }

    if (mNextSpecifierInterpolation)
    {
        mNextSpecifierInterpolation->mPreviousSpecifierInterpolation=nullptr;
        mNextSpecifierInterpolation->changeFirstExtremity(mAssociatedVertex);
    }
}


void ReosVertexZSpecifierInterpolation::calculateZValue() const
{
    std::list<VertexPointer> vertices=verticesList();

    double distance=0;
    auto pv=vertices.begin();
    auto p=pv;
    p++;
    while(p!=vertices.end())
    {
        distance+=(*pv)->distanceFrom(*(*p));
        pv=p;
        p++;
    }


    double firstZValue=vertices.front()->z();
    double gap=firstZValue-vertices.back()->z();
    double slope=gap/distance;

    pv=vertices.begin();
    p=pv;
    p++;
    auto lp=vertices.end();
    lp--;
    double cumul=0;
    while(p!=lp)
    {
        auto currentSpecifier=static_cast<ReosVertexZSpecifierInterpolation*>((*p)->zSpecifier());
        if ((*p)!=mAssociatedVertex)
            std::lock_guard<std::mutex> g(currentSpecifier->mMutex);
        currentSpecifier->mDirty=false;
        cumul+=(*pv)->distanceFrom(*(*p));
        currentSpecifier->mZValue=firstZValue-cumul*slope;
        pv=p;
        p++;
    }

}

std::list<VertexPointer> ReosVertexZSpecifierInterpolation::verticesList() const
{
    std::list<VertexPointer> vertices;
    vertices.push_back(mAssociatedVertex);

    //go previously to retrieve the previous vertices
    ReosVertexZSpecifierInterpolation *currentSpecifier=mPreviousSpecifierInterpolation;
    while(currentSpecifier)
    {
        vertices.push_front(currentSpecifier->mAssociatedVertex);
        currentSpecifier= currentSpecifier->mPreviousSpecifierInterpolation;
    }

    //go next to retrieve the nect vertices
    currentSpecifier=mNextSpecifierInterpolation;
    while(currentSpecifier)
    {
        vertices.push_back(currentSpecifier->mAssociatedVertex);
        currentSpecifier=currentSpecifier->mNextSpecifierInterpolation;
    }

    vertices.push_front(mFirstExtremity);
    vertices.push_back(mSecondExtremity);

    return vertices;
}

void ReosVertexZSpecifierOtherVertexAndGapFactory::setGap(double gap)
{
    mGap=gap;
}
