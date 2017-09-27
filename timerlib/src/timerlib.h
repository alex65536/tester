/*
  timerlib.h - Header for libtimer.a

  Copyright (C) 2017 Alexander Kernozhitsky <sh200105@mail.ru>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
*/

/*
  This library uses some sources from Ejudge project <ejudge.ru>
*/

#ifndef TIMERLIB_H_INCLUDED
#define TIMERLIB_H_INCLUDED

typedef enum
{
	TR_OK,
	TR_TIME_LIMIT,
	TR_REAL_TIME_LIMIT,
	TR_MEMORY_LIMIT,
	TR_RUNTIME_ERROR,
	TR_RUN_FAIL
} TIMER_RESULT;

extern TIMER_RESULT launch_timer(
	const char* working_dir,
	const char* exe_name,
	const char* stdin_redir,
	const char* stdout_redir,
	const char* stderr_redir,
	int time_limit,
	int realtime_limit,
	int set_memory_limit,
	int memory_limit,
	int* work_time,
	int* work_realtime,
	int* work_memory,
	int* exit_code
);

extern void gui_mode();

extern void init_timer();

#endif //TIMERLIB_H_INCLUDED

