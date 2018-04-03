package com.ml.services.metric;

import java.util.Collection;

import com.ml.model.Metric;

public interface MetricService {

	Collection<Metric> getAllMetrics();

	Metric getByDia(String dia);

	Metric getById(String id);

}