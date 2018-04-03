package com.ml.services.metric.impl;

import java.util.Collection;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.ml.model.Metric;

import com.ml.repositorys.MetricRepository;
import com.ml.services.metric.MetricService;

@Service
public class MetricServiceImpl implements MetricService {

	@Autowired
	private MetricRepository repository;

	@Override
	public Collection<Metric> getAllMetrics() {
		Collection<Metric> result = (Collection<Metric>) repository.findAll();
		return result;
	}

	@Override
	public Metric getById(String id) {

	    return repository.findOne(id);
	}

	@Override
	public Metric getByDia(String dia) {

		return repository.findByDia(dia);
	}


}
