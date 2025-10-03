# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: ioleinik <ioleinik@student.42wolfsburg.de> +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2021/05/14 18:13:08 by ioleinik          #+#    #+#              #
#    Updated: 2021/11/16 07:42:29 by ioleinik         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

CC		= gcc
CFLAGS	= -Wall -Wextra -Werror -g

AR		= ar rcs
RM		= rm -f

NAME	= libft.a
INCL	= libft.h

# Colors for output
COLOR_GREEN = \033[0;32m
COLOR_BLUE = \033[0;34m
COLOR_RESET = \033[0m

SRC		= ft_atoi.c ft_bzero.c ft_isalnum.c ft_isalpha.c ft_isascii.c \
			ft_isdigit.c ft_isprint.c ft_memccpy.c ft_memchr.c ft_memcmp.c \
			ft_memcpy.c ft_memmove.c ft_memset.c ft_strchr.c ft_strlcat.c \
			ft_strlcpy.c ft_strlen.c ft_strncmp.c ft_strnstr.c ft_strrchr.c \
			ft_tolower.c ft_toupper.c ft_calloc.c ft_strdup.c ft_substr.c \
			ft_strjoin.c ft_strtrim.c ft_split.c ft_itoa.c ft_strmapi.c \
			ft_putchar_fd.c ft_putstr_fd.c ft_putendl_fd.c ft_putnbr_fd.c \
			ft_lstnew.c ft_lstadd_front.c ft_lstsize.c ft_lstlast.c ft_lstadd_back.c \
			ft_lstdelone.c ft_lstclear.c ft_lstiter.c ft_lstmap.c ft_putchar.c \
			ft_putstr.c ft_memdel.c ft_strnew.c \
			ft_striteri.c ft_wordcount.c ft_terror.c ft_straddchar.c \
			ft_atoi_base.c \
			ft_split_free.c ft_strcmp.c ft_strarrlen.c ft_strarrdup.c \
			ft_addstrstrarr.c \
			get_next_line.c ft_strarrnew.c

OBJ		= ${SRC:.c=.o}

all:		$(NAME)

$(NAME):	$(OBJ)
			$(AR) $(NAME) $(OBJ)

clean:		
			@$(RM) $(OBJ)

fclean:		clean
			@$(RM) $(NAME)

re:			fclean all

# Test targets
TEST_DIR	= tests
TEST_NAME	= test_libft
TEST_SRC	= $(TEST_DIR)/test_main.c
TEST_FLAGS	= -I.

test:		$(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME)
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-char:	$(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) char
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-memory: $(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) memory
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-string: $(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) string
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-manipulation: $(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) manipulation
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-conversion: $(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) conversion
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-output: $(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) output
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-list:	$(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) list
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-utility: $(NAME)
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			./$(TEST_NAME) utility
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-clean:
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-fclean: test-clean
			@$(RM) $(NAME)

# Norminette checks
norm:
			@echo "$(COLOR_BLUE)Checking norminette compliance...$(COLOR_RESET)"
			@norminette *.c *.h | grep -v "OK!" || echo "$(COLOR_GREEN)✓ All files pass norminette!$(COLOR_RESET)"

norm-verbose:
			@echo "$(COLOR_BLUE)Detailed norminette check...$(COLOR_RESET)"
			@norminette *.c *.h

test-norm: norm test
			@echo "$(COLOR_GREEN)All tests passed including norminette compliance!$(COLOR_RESET)"

# Valgrind memory leak checks
valgrind: $(NAME)
			@echo "$(COLOR_BLUE)Running Valgrind memory leak detection...$(COLOR_RESET)"
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			@valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --error-exitcode=1 \
				--suppressions=/dev/null ./$(TEST_NAME) 2>&1 | \
				grep -E "(ERROR SUMMARY|LEAK SUMMARY|definitely lost|indirectly lost|possibly lost)" || \
				echo "$(COLOR_GREEN)✓ No memory leaks detected!$(COLOR_RESET)"
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

valgrind-full: $(NAME)
			@echo "$(COLOR_BLUE)Running detailed Valgrind analysis...$(COLOR_RESET)"
			$(CC) $(CFLAGS) $(TEST_FLAGS) $(TEST_SRC) $(NAME) -o $(TEST_NAME)
			valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --verbose ./$(TEST_NAME)
			@$(RM) $(TEST_NAME)
			@$(RM) $(OBJ)

test-valgrind: valgrind norm test
			@echo "$(COLOR_GREEN)All tests passed including memory leak detection and norminette compliance!$(COLOR_RESET)"

.PHONY: all clean fclean re test test-char test-memory test-string test-manipulation test-conversion test-output test-list test-utility test-clean test-fclean norm norm-verbose test-norm valgrind valgrind-full test-valgrind