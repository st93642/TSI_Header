/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   get_next_line.c                                    :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: ioleinik <ioleinik@student.42wolfsburg.de> +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2021/06/06 20:24:31 by ioleinik          #+#    #+#             */
/*   Updated: 2021/11/17 21:15:13 by ioleinik         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "libft.h"

static int	read_line_content(int fd, char *buffer, int *i)
{
	int		rd;
	char	ch;

	rd = 1;
	while (rd > 0)
	{
		rd = read(fd, &ch, 1);
		if (rd <= 0 || ch == '\n')
			break ;
		buffer[(*i)++] = ch;
	}
	return (rd);
}

int	get_next_line(int fd, char **line)
{
	int		rd;
	int		i;
	char	*buffer;

	if (fd < 0 || !line)
		return (-1);
	buffer = malloc(1000);
	if (!buffer)
		return (-1);
	*line = buffer;
	i = 0;
	rd = read_line_content(fd, buffer, &i);
	buffer[i] = '\0';
	if (rd > 0)
		return (1);
	else if (i > 0)
		return (0);
	else
		return (rd);
}
