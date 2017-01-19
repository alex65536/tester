/*
  Copyright (C) 2017 Kernozhitsky Alexander <sh200105@mail.ru>

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

#include "../timerlib.h"
#include "exec.h"

TIMER_RESULT launch_timer(
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
		int* exit_code)
{
	// create task
	tpTask task = task_New();
	if (task == NULL)
		return TR_RUN_FAIL;
	// some defines for convenince
	#define secure_task_op(op) \
		if (op) \
		{ \
			task_Delete(task); \
			return TR_RUN_FAIL; \
		}

	#define file_name(str) \
		((*(str) == '\0') ? "/dev/null" : (str))

	#define file_rights(str) \
		((*(str) == '\0') ? TSK_WRITE : TSK_REWRITE)

	// set params
	secure_task_op(task_SetWorkingDir(task, working_dir))
	secure_task_op(task_SetPath(task, exe_name))
	secure_task_op(task_SetMaxTimeMillis(task, time_limit))
	secure_task_op(task_SetMaxRealTimeMillis(task, realtime_limit))
	secure_task_op(task_EnableMemoryLimitError(task))
	secure_task_op(task_SetStackSize(task, set_memory_limit))
	secure_task_op(task_SetDataSize(task, set_memory_limit))
	secure_task_op(task_SetVMSize(task, set_memory_limit))
	// set redirections
	// stdin
	secure_task_op(task_SetRedir(task,
	                             0,
	                             TSR_FILE,
	                             file_name(stdin_redir),
	                             TSK_READ))
	// stdout
	secure_task_op(task_SetRedir(task,
	                             1,
	                             TSR_FILE,
	                             file_name(stdout_redir),
	                             file_rights(stdout_redir),
	                             TSK_FULL_RW))
	// stderr
	secure_task_op(task_SetRedir(task,
	                             2,
	                             TSR_FILE,
	                             file_name(stderr_redir),
	                             file_rights(stderr_redir),
	                             TSK_FULL_RW))
	// run process
	secure_task_op(task_Start(task))
	if (task_Wait(task) == NULL)
		return TR_RUN_FAIL;
	// process results
	TIMER_RESULT result = TR_OK;
	// check exitcode
	*exit_code = task_ExitCode(task);
	if (*exit_code != 0)
		result = TR_RUNTIME_ERROR;
	// check memory
	*work_memory = task_GetMemoryUsed(task);
	if (*work_memory >= memory_limit || task_IsMemoryLimit(task))
	{
		*work_memory = memory_limit;
		result = TR_MEMORY_LIMIT;
	}
	// check time
	*work_time = task_GetRunningTime(task);
	if (*work_time >= time_limit || task_IsTimeout(task))
	{
		*work_time = time_limit;
		result = TR_TIME_LIMIT;
	}
	// check real time
	*work_realtime = task_GetRunningTime(task);
	if (*work_realtime >= realtime_limit || task_IsRealTimeout(task))
	{
		*work_realtime = realtime_limit;
		result = TR_REAL_TIME_LIMIT;
	}
	// delete task and return result
	task_Delete(task);
	return result;
	// undef defines
	#undef file_rights
	#undef file_name
	#undef secure_task_op
}

void init_timer()
{
}
