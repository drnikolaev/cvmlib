//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2023
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "cvm.h"
#include <algorithm>

#if !defined(_MSC_VER)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wexceptions"
#endif

extern "C" {
    void __stdcall XERBLA(const char* szSubName,
    #if defined(CVM_PASS_STRING_LENGTH_TO_FTN_SUBROUTINES)
                        const tint,
    #endif
                        const tint* pnParam)
    {
        throw cvm::cvmexception(CVM_WRONGMKLARG2, *pnParam, szSubName);
    }
}

#if !defined(_MSC_VER)
#pragma GCC diagnostic pop
#endif

CVM_NAMESPACE_BEG

#if defined(CVM_USE_POOL_MANAGER)

//! @cond INTERNAL
CriticalSection::CriticalSection()
    : mbOK(false),
#if defined(WIN32) || defined(_WIN32)
    mMutex(0)
#else                                                       // POSIX Threads library assumed
    mMutex(), mMutexAttr()
#endif
{
#if defined(WIN32) || defined(_WIN32)
    mMutex = ::CreateMutex(nullptr, FALSE, nullptr);
    mbOK = mMutex != nullptr;
#else
    if (pthread_mutexattr_init(&mMutexAttr) == 0 &&
        pthread_mutexattr_setpshared(&mMutexAttr, PTHREAD_PROCESS_PRIVATE) == 0 &&
        pthread_mutex_init(&mMutex, &mMutexAttr) == 0)
    {
        mbOK = true;
    }
#endif
}

CriticalSection::~CriticalSection()
{
#if defined(WIN32) || defined(_WIN32)
    if (mbOK)
    {
        ::CloseHandle(mMutex);
    }
#else
    pthread_mutexattr_destroy(&mMutexAttr);
    pthread_mutex_destroy(&mMutex);
#endif
}

void CriticalSection::enter()
{
#if defined(WIN32) || defined(_WIN32)
    if (!mbOK)
    {
        throw cvmexception(CVM_SEMAPHOREERROR);
    }
    ::WaitForSingleObject(mMutex, INFINITE);
#else
    if (!mbOK || pthread_mutex_lock(&mMutex) != 0)
    {
        throw cvmexception(CVM_SEMAPHOREERROR);
    }
#endif
}

void CriticalSection::leave()
{
#if defined(WIN32) || defined(_WIN32)
    if (!mbOK)
    {
        throw cvmexception(CVM_SEMAPHOREERROR);
    }
    ::ReleaseMutex(mMutex);
#else
    if (!mbOK || pthread_mutex_unlock(&mMutex) != 0)
    {
        throw cvmexception(CVM_SEMAPHOREERROR);
    }
#endif
}
//! @endcond
#endif

#if defined(CVM_USE_POOL_MANAGER)

MemoryPool gPool;
CriticalSection gCS;

CVM_API tbyte* _cvmMalloc(size_t nBytes)
{
    Lock l(gCS);
    return gPool.Malloc(nBytes);
}

CVM_API tbyte* _cvmAddRef(const tbyte* pD)
{
    Lock l(gCS);
    return gPool.AddRef(pD);
}

CVM_API tint _cvmFree(tbyte*& pD)
{
    Lock l(gCS);
    return gPool.Free(pD);
}


#define CVM_PAGE_SIZE (0x1000)

size_t _up_value(size_t n)                                     // the least power of 2 multiplied by 2
{
    if (n < CVM_PAGE_SIZE) {                                    // let small objects be in one page
        n = CVM_PAGE_SIZE;
    }
    else {
        static const size_t one(1);
        size_t i = 0;
        while (n >> i) ++i;
        if (i && (n & ((one << (i - one)) - one))) ++i;         // obey warning C4554 :)
        n = one << i;
    }
    return n;
}

MemoryPool::MemoryPool()
{
}

MemoryPool::~MemoryPool()
{
    Clear();
}

void MemoryPool::Clear()
{
    std::for_each(mOutBlocks.rbegin(), mOutBlocks.rend(), MemoryPool::DeletePtr());
    mOutBlocks.clear();
}

tbyte* MemoryPool::Malloc(size_t nBytes)
{
    if (nBytes == 0) return nullptr;
    tbyte* pB = mMemoryBlocks.GetFreeBlock(nBytes);
    if (pB == nullptr) {    // There is no suitable memory block. Let's create a new one.
        const size_t nUpBytes = _up_value(nBytes);
        const size_t nRest    = nUpBytes - nBytes;
        try {
            pB = ::new tbyte[nUpBytes];
        }
        catch(const std::bad_alloc&) {
        }
        if (pB == nullptr) {
            throw(cvmexception(CVM_OUTOFMEMORY, nBytes));
        }
        mOutBlocks.push_back(pB);
        mMemoryBlocks.AddPair(pB, nBytes, nRest);
    }
    return pB;
}

tbyte* MemoryPool::AddRef(const tbyte* pD)
{
    return mMemoryBlocks.AddRef(pD);
}

tint MemoryPool::Free(tbyte*& pToFree)
{
    tint nRefCounter = mMemoryBlocks.FreeBlock(pToFree);
    if (!nRefCounter)
    {
        pToFree = nullptr;
    }
    return nRefCounter;
}


void MemoryBlocks::AddBlock(tbyte* pBlock, size_t nBytes, bool bOccupied)
{
    if (!bOccupied)                                                             // Add freed block
    {
        itr_FreeIt j;
        itr_Blocks i = mBlocks.upper_bound(pBlock);
        itr_Blocks i_next = i;
                                                                                // Is there upper neighboring memory block?
        if (i != mBlocks.end())
        {
            tbyte* pUpperBlock = i->first;
            j = mFreeIt.find(pUpperBlock);
            if (j != mFreeIt.end() && pBlock + nBytes == pUpperBlock)           // Yes. It's free and will be concatenated
            {
                nBytes += i->second.mnSize;
                ++i_next;
                mBlocks.erase(i);
                i = i_next;
                mFreeBs.erase(j->second);
                mFreeIt.erase(j);
            }
        }
                                                                                // Is there lower neighboring memory block?
        if (i != mBlocks.begin() && mBlocks.size() > 0)
        {
            --i;
            tbyte* pLowerBlock = i->first;
            const size_t nLowerBytes = i->second.mnSize;
            j = mFreeIt.find(pLowerBlock);
            if (j != mFreeIt.end() && pLowerBlock + nLowerBytes == pBlock)      // Yes. It's free and will be concatenated
            {
                pBlock = pLowerBlock;
                nBytes += nLowerBytes;
                mBlocks.erase(i);
                mFreeBs.erase(j->second);
                mFreeIt.erase(j);
            }
        }
        mFreeIt[pBlock] = mFreeBs.insert(std::pair<size_t, tbyte*>(nBytes, pBlock));
    }

    mBlocks.insert(std::pair<tbyte*, BlockProperty>(pBlock, BlockProperty(nBytes, 1)));
}

tint MemoryBlocks::FreeBlock(tbyte* pBlock)
{
    tint nRefCounter = 0;
    itr_Blocks i = mBlocks.find(pBlock);

    if (i != mBlocks.end())
    {
        if (mFreeIt.find(pBlock) == mFreeIt.end())
        {
            nRefCounter = -- i->second.mnRefCount;
            if (nRefCounter <= 0)
            {
                const size_t nBytes = i->second.mnSize;
                mBlocks.erase(i);
                AddBlock(pBlock, nBytes, false);                               // return free block to the pool
            }
        }
#ifdef CVM_DEBUG
        else
        {
            assert(mFreeIt.find(pBlock) == mFreeIt.end());
        }
#endif
    }
    else
    {
        nRefCounter = -1;                                                       // foreign array.
    }

    return nRefCounter;
}

tbyte* MemoryBlocks::GetFreeBlock(size_t nBytes)
{
    tbyte* pBlock = nullptr;
    if (mFreeBs.size() > 0)
    {
        // Is there a suitable memory block?
        itr_FreeBs i = mFreeBs.lower_bound(nBytes);

        // Yes. Let's use it.
        if (i != mFreeBs.end())
        {
            const size_t nRest = i->first - nBytes;
            pBlock = i->second;

            mFreeBs.erase(i);
            if (mFreeIt.size() > 0 && mFreeIt.find(pBlock) != mFreeIt.end())
            {
                mFreeIt.erase(pBlock);
            }
            if (mBlocks.size() > 0 && mBlocks.find(pBlock) != mBlocks.end())
            {
                mBlocks.erase(pBlock);
            }

            AddPair(pBlock, nBytes, nRest);
        }
    }
    return pBlock;
}

tbyte* MemoryBlocks::AddRef(const tbyte* pcBlock)
{
    tbyte* pBlock = const_cast<tbyte*>(pcBlock);
    itr_Blocks i = mBlocks.find(pBlock);
    if (i != mBlocks.end())
    {
        ++ i->second.mnRefCount;
    }
    else
    {
        // This is a foreign array. Leave it alone.
    }
    return pBlock;
}


#ifdef CVM_DEBUG
void MemoryBlocks::Assert(const void* pvBlock, size_t nBytes)
{
    tbyte* pBlock = (tbyte*) const_cast<void*>(pvBlock);
    itr_Blocks i = mBlocks.find(pBlock);
    if (i != mBlocks.end()) {
        const size_t nSize = i->second.mnSize;
        assert(nSize >= nBytes);
    }
    else {
        tbyte* pB;
        size_t nB;
        itr_Blocks end = mBlocks.end();
        for (i = mBlocks.begin(); i != end; ++i) {
            pB = i->first;
            nB = i->second.mnSize;
            if (pBlock >= pB && pBlock < pB + nB) {
                tbyte* pBase = pB + nB;
                tbyte* pTest = pBlock + nBytes;
                assert(pTest <= pBase);
            }
        }
    }
}
#endif

void MemoryBlocks::AddPair(tbyte* pBlock, size_t nBytes, size_t nRest)
{
    AddBlock(pBlock, nBytes, true);                            // occupied block...
    if (nRest > 0) {
        AddBlock(pBlock + nBytes, nRest, false);               // ...and the rest free block
    }
}
#endif // !CVM_USE_POOL_MANAGER

#if defined(CVM_USE_POOL_MANAGER) && defined(CVM_DEBUG)
CVM_API void _cvm_assert(const void* pvBlock, size_t nBytes)
{
    Lock l(gCS);
    gPool.Assert(pvBlock, nBytes);
}
#else
CVM_API void _cvm_assert(const void*, size_t)
{
}
#endif  // CVM_USE_POOL_MANAGER && CVM_DEBUG

CVM_API void cvmExit()
{
#ifdef CVM_USE_POOL_MANAGER
    gPool.Clear();
#endif
}

CVM_NAMESPACE_END
