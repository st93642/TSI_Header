/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   ft_strarrlen.c                                     :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: ioleinik <ioleinik@student.42wolfsburg.de> +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2021/10/10 06:49:09 by ioleinik          #+#    #+#             */
/*   Updated: 2021/10/10 17:44:25 by ioleinik         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "libft.h"

int	ft_strarrlen(char **src)
{
	int	i;

	if (!src)
		return (0);
	i = 0;
	while (src[i])
		i++;
	return (i);
}
