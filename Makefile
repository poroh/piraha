#
# Copyright (c) 2019 Dmitry Poroh
# All rights reserved.
# Distributed under the terms of the MIT License. See the LICENSE file.
#
#

run:
	ERL_AFLAGS="-kernel logger_level debug" rebar3 shell
