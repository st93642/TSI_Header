/* ************************************************************************** */
/*                                                                            */
/*                                                        :::      ::::::::   */
/*   ft_strarrdup.c                                     :+:      :+:    :+:   */
/*                                                    +:+ +:+         +:+     */
/*   By: ioleinik <ioleinik@student.42wolfsburg.de> +#+  +:+       +#+        */
/*                                                +#+#+#+#+#+   +#+           */
/*   Created: 2021/10/10 06:52:38 by ioleinik          #+#    #+#             */
/*   Updated: 2021/10/10 10:16:59 by ioleinik         ###   ########.fr       */
/*                                                                            */
/* ************************************************************************** */

#include "libft.h"

char	**ft_strarrdup(char **src)
{
	char	**dup;
	int		i;

	if (!src)
		return (NULL);
	i = 0;
	dup = (char **)malloc(sizeof(char *) * (ft_strarrlen(src) + 1));
	if (!dup)
		return (NULL);
	while (src[i])
	{
		dup[i] = ft_strdup(src[i]);
		if (!dup[i])
		{
			while (--i >= 0)
				free(dup[i]);
			free(dup);
			return (NULL);
		}
		i++;
	}
	dup[i] = NULL;
	return (dup);
}
