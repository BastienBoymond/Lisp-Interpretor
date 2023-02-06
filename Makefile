##
## EPITECH PROJECT, 2023
## Untitled (Workspace)
## File description:
## Makefile
##

local_path	:=	$(shell stack path --local-install-root)
executable	:=	$(local_path)/bin
NAME	=	glados

all:
	@echo "Compiling..."
	@stack build --ghc-options "-O0"
	@echo "Compilation done."
	@stack path --local-install-root
	cp $(executable)/glados-exe ./$(NAME)

clean:
	@echo "Cleaning..."
	@stack clean
	@echo "Cleaning done."

fclean:
	@echo "Cleaning..."
	@rm -f $(NAME)
	@rm -rf .stack-work
	@echo "Cleaning done."

tests: all
	@echo "Testing..."
	@stack test

ftests: all
	@echo "functionnal Testing..."
	@./test/launch_tests.sh

re: fclean all
