//                  CVM Class Library
//                  http://cvmlib.com
//
//          Copyright Sergei Nikolaev 1992-2014
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include "StdAfx.h"
#include "test.h"

int main(int argc, char* argv[])
{
    int i;
    int nThreads = 1;
    int nRuns = 1;

    for (i = 1; i < argc; ++i)
    {
        if (argv[i] != NULL && strlen(argv[i]) > 2)
        {
            char cSep = argv[i][0];
            char cKey = argv[i][1];
            if (strchr ("-/", cSep))
            {
                if (cKey == 't' || cKey == 'T')
                {
                    nThreads = (int) atol(argv[i] + 2);
                    if (nThreads <= 0) nThreads = 1;
                }
                if (cKey == 'r' || cKey == 'R')
                {
                    nRuns = (int) atol(argv[i] + 2);
                    if (nRuns <= 0) nRuns = 1;
                }
            }
        }
    }

#if !defined (CVM_TEST_WIN_THREADS)
    if (nThreads > 2)
    {
        std::cout << "Warning! Running 2 threads only!" << std::endl;
    }
#endif

    clock_t start, finish;
    start = clock();

    for (i = 0; i < nRuns; ++i)
    {
#if defined (CVM_TEST_WIN_THREADS)

        unsigned int threadID = 0;

        if (nThreads > 1)
        {
            int i;
            int nThreads2 = nThreads * 2;
            std::vector<HANDLE> tha(nThreads2);

            for (i = 0; i < nThreads2; i+=2)
            {
                tha[i] = (HANDLE) _beginthreadex (NULL, 0, TestBody, NULL, 0, &threadID);
                tha[i+1] = (HANDLE) _beginthreadex (NULL, 0, TestFunBody, NULL, 0, &threadID);
            }

            WaitForMultipleObjects(nThreads2, &tha[0], TRUE, INFINITE);

            for (i = nThreads2 - 1; i >= 0; i--)
            {
                ::CloseHandle(tha[i]);
            }
        }
        else
        {
            TestBody(NULL);
            TestFunBody(NULL);
        }

#else

        pthread_t tid = 0, tidf = 0;
        void* tr = NULL;

        if (nThreads > 1)
        {
            LockIt<gCS> l;
            if (pthread_create (&tid, NULL, TestBody, NULL) != 0)
            {
                std::cout << "Failed to create thread!" << std::endl;
            }
            else
            {
                std::cout << tid << std::endl;
            }
            if (pthread_create (&tidf, NULL, TestFunBody, NULL) != 0)
            {
                std::cout << "Failed to create thread!" << std::endl;
            }
            else
            {
                std::cout << tidf << std::endl;
            }
        }

        TestBody(NULL);
        TestFunBody(NULL);

        if (nThreads > 1)
        {
            if (pthread_join (tid, &tr) != 0)
            {
                std::cout << "Failed to join thread!" << std::endl;
            }
            if (pthread_join (tidf, &tr) != 0)
            {
                std::cout << "Failed to join thread!" << std::endl;
            }
        }
#endif
    }

    finish = clock();
    double duration = (double) (finish - start) / CLOCKS_PER_SEC;
    std::cout << "TOTAL TIME " << duration << " sec." << std::endl;
    return exit_code;
}


